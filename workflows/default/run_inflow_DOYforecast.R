# Script to generate a simple inflow forecast of temp, salt and discharge
#

source('R/inflow_DOYmodel.R')

# Read in targets
inflow_targets <- read_csv(cleaned_inflow_file)

# What dates should the forecast be generated for
start <- as_date(config$run_config$forecast_start_datetime) + days(1)
end <-  as_date(start) + days(config$run_config$forecast_horizon)

forecast_dates <- seq.Date(from = start, 
                           to = end, by = 'day')

# Generate the inflow forecasts
inflow_forecast <- map_dfr(.x = c('FLOW', 'SALT', 'TEMP'), 
                            ~inflows_DOY(df= inflow_targets,
                                         var = .x, forecast_dates = forecast_dates, reference_datetime = start))

# Currently flare needs an outflow so we will just duplicate the inflow forecast
outflow_forecast <- inflow_forecast |> 
  mutate(flow_type = 'outflow')

combined_flow <- bind_rows(inflow_forecast, outflow_forecast)

# Write the inflow forecast out - to local directory or s3 depending on config
inflow_forecast_path <- file.path(config$inflow$forecast_inflow_model, 
                                  forecast_site, '0', 
                                  lubridate::as_date(config$run_config$forecast_start_datetime))


if(config$run_config$use_s3){
  FLAREr:::arrow_env_vars()
  inflow_s3 <- arrow::s3_bucket(bucket = file.path(config$s3$inflow_drivers$bucket, inflow_forecast_path), 
                                endpoint_override = config$s3$inflow_drivers$endpoint)
  on.exit(FLAREr:::unset_arrow_vars(vars))
} else{
  inflow_s3 <- arrow::SubTreeFileSystem$create(file.path(config$inflow$local_directory, inflow_forecast_path))
}

arrow::write_dataset(combined_flow, path = inflow_s3)
