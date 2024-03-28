## function for creating inflow forecasts using xgboost model

run_inflow_model <- function(site_id, 
                             forecast_start_datetime,
                             use_s3_inflow = FALSE, 
                             inflow_bucket = NULL,
                             inflow_endpoint = NULL,
                             inflow_local_directory = NULL, 
                             forecast_horizon = NULL, 
                             inflow_model = NULL){
  
  if((!is.null(forecast_start_datetime)) && (forecast_horizon > 0)){
    
    forecast_date <- lubridate::as_date(forecast_start_datetime) - lubridate::days(1)
    forecast_hour <- lubridate::hour(forecast_start_datetime)
    
    ## pull in future NOAA data 
    
    met_s3_future <- arrow::s3_bucket(file.path("bio230121-bucket01/flare/drivers/met/gefs-v12/stage2",paste0("reference_datetime=",noaa_date),paste0("site_id=",site_id)),
                                      endpoint_override = endpoint,
                                      anonymous = TRUE)
    # old method
    # met_s3_future <- arrow::s3_bucket(file.path(config$s3$drivers$bucket,
    #                                             paste0("stage2/parquet/0/", forecast_date),
    #                                             paste0("", site_id)),
    #                                   endpoint_override = endpoint,
    #                                   anonymous = TRUE)
    
    df_future <- arrow::open_dataset(met_s3_future) |> 
      filter(variable %in% c("precipitation_flux","air_temperature")) |> 
      collect() |> 
      rename(ensemble = parameter) |> 
      mutate(variable = ifelse(variable == "precipitation_flux", "precipitation", variable),
             variable = ifelse(variable == "air_temperature", "temperature_2m", variable),
             prediction = ifelse(variable == "temperature_2m", prediction - 273.15, prediction))
    
    min_datetime <- min(df_future$datetime)
    
    
    ## pull in past NOAA data
    met_s3_past <- arrow::s3_bucket(paste0("bio230121-bucket01/flare/drivers/met/gefs-v12/stage3/site_id=",site_id),
                                    endpoint_override = endpoint,
                                    anonymous = TRUE)
    
    years_prior <- reference_datetime - lubridate::days(1825) # 5 years
    
    df_past <- arrow::open_dataset(met_s3_past) |> 
      #select(datetime, parameter, variable, prediction) |> 
      filter(variable %in% c("precipitation_flux","air_temperature"),
             ((datetime <= min_datetime  & variable == "precipitation_flux") | 
                datetime < min_datetime  & variable == "air_temperature"),
             datetime > years_prior) |> 
      collect() |> 
      rename(ensemble = parameter) |> 
      mutate(variable = ifelse(variable == "precipitation_flux", "precipitation", variable),
             variable = ifelse(variable == "air_temperature", "temperature_2m", variable),
             prediction = ifelse(variable == "temperature_2m", prediction - 273.15, prediction)) |> 
      select(-reference_datetime)
    
    
    # combine past and future noaa data
    df_combined <- bind_rows(df_future, df_past) |> 
      arrange(variable, datetime, ensemble)
    
    forecast_precip <- df_combined |> 
      filter(variable == 'precipitation') |> 
      summarise(precip_hourly = median(prediction, na.rm = TRUE), .by = c("datetime")) |> # get the median hourly precip across all EMs
      mutate(date = lubridate::as_date(datetime)) |> 
      summarise(precip = sum(precip_hourly, na.rm = TRUE), .by = c("date")) |> # get the total precip for each day
      mutate(sevenday_precip = RcppRoll::roll_sum(precip, n = 7, fill = NA,align = "right")) |> 
      mutate(doy = lubridate::yday(date))
    
    forecast_temp <- df_combined |> 
      filter(variable == 'temperature_2m') |> 
      summarise(temp_hourly = median(prediction, na.rm = TRUE), .by = c("datetime")) |> # get the median hourly temp across all EMs
      mutate(date = lubridate::as_date(datetime)) |> 
      summarise(temperature = median(temp_hourly, na.rm = TRUE), .by = c("date")) # get median temp across hours of the day
    
    forecast_met <- forecast_precip |> 
      right_join(forecast_temp, by = c('date'))
    
    
    
    ## RUN PREDICTIONS
    #sensorcode_df <- read_csv('configuration/default/sensorcode.csv', show_col_types = FALSE)
    
    inflow_targets <- read_csv(file.path('targets', config$location$site_id, paste0(config$location$site_id,"-targets-inflow.csv")), show_col_types = FALSE)
    
    ## RUN FLOW PREDICTIONS
    print('Running Flow Inflow Forecast')
    
    flow_targets <- inflow_targets |>
      filter(variable == 'FLOW') |> 
      rename(date = datetime, total_flow = observation)
    
    flow_predictions <- run_inflow_flow_model(met_df = forecast_met, 
                                              met_past_df = df_past, 
                                              met_combined = df_combined, 
                                              targets_df = flow_targets)
    
    ## RUN TEMPERATURE PREDICTIONS
    print('Running Temperature Inflow Forecast')
    temp_targets <- inflow_targets |>
      filter(variable == 'TEMP') |> 
      rename(date = datetime, water_temperature = observation)
    
    temp_predictions <- run_inflow_temperature_model(met_df = forecast_met, 
                                                     met_past_df = df_past, 
                                                     met_combined = df_combined, 
                                                     targets_df = temp_targets)
    
    ## RUN SALINITY PREDICTIONS
    print('Running Salinity Inflow Forecast')
    salt_targets <- inflow_targets |>
      filter(variable == 'SALT') |> 
      rename(date = datetime, salinity = observation)
    
    salt_predictions <- run_inflow_salinity_model(met_df = forecast_met, 
                                                  met_past_df = df_past, 
                                                  met_combined = df_combined, 
                                                  targets_df = salt_targets)
    
    
    inflow_combined <- bind_rows(flow_predictions, temp_predictions, salt_predictions)
    
    outflow_df <- inflow_combined
    outflow_df$flow_type <- 'outflow'
    
    flow_combined <- bind_rows(inflow_combined, outflow_df)
    
    #flow_combined <- inflow_combined
    
    if (forecast_horizon > 0) {
      inflow_forecast_path <- file.path(inflow_model, site_id, forecast_hour, lubridate::as_date(forecast_start_datetime))
    }else {
      inflow_forecast_path <- NULL
    }
    
    if(use_s3_inflow){
      FLAREr:::arrow_env_vars()
      inflow_s3 <- arrow::s3_bucket(bucket = file.path(inflow_bucket, inflow_forecast_path), endpoint_override = inflow_endpoint)
      on.exit(FLAREr:::unset_arrow_vars(vars))
    }else{
      inflow_s3 <- arrow::SubTreeFileSystem$create(file.path(inflow_local_directory, inflow_forecast_path))
    }
    
    arrow::write_dataset(flow_combined, path = inflow_s3)
    
    inflow_local_files <- list.files(file.path(inflow_local_directory, inflow_forecast_path), full.names = TRUE, recursive = TRUE)
    
    print("Inflow files processed...")
    
    
  }else{
    message("nothing to forecast")
    inflow_local_files <- NULL
  }# end if statement 
  
  return(inflow_local_files)
}
