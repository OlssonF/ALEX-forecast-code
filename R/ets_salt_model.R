ets_salt_model <- function(salt_targets, horizon, df_future, config){
  
  # salt_targets <- inflow_targets |>
  #   dplyr::filter(variable == 'SALT') |> 
  #   rename(date = datetime) |> 
  #   mutate(observation = na.interp(observation))
  
  salt_fit <- forecast::ets(as.ts(salt_targets$observation))
  
  #horizon <- as.numeric(as.Date(max(forecast_met$date)) - as.Date(config$run_config$forecast_start_datetime))
  
  #salt_forecast_values <- forecast(salt_fit, h = horizon, level = 0.03) ## taken as sigma (1 SD) from salt_fit
  
  salt_predictions <- as.data.frame(forecast(salt_fit, h = horizon, level = 0.03)) |> 
    mutate(datetime = seq.Date(as.Date(config$run_config$forecast_start_datetime) + days(1),as.Date(max(df_future$datetime, na.rm = T)), by = 'day')) |> 
    mutate(sigma = `Hi 3`- `Point Forecast`) |>
    mutate(mu = as.numeric(`Point Forecast`)) |>
    select(datetime, mu, sigma)
  
  #salt_build <- salt_predictions |> select(datetime)
  n_members <- max(df_future$ensemble)
  
  salt_build <- data.frame()
  
  for (i in seq.int(1:nrow(salt_predictions))){
    salt_em_values <- rnorm(n_members+1, mean = salt_predictions$mu[i], sd = salt_predictions$sigma[i])
    
    salt_em_df <- data.frame(datetime = as.Date(salt_predictions$datetime[i]), 
                             ensemble = seq.int(0:n_members) -1, 
                             prediction = salt_em_values)
    
    salt_build <- dplyr::bind_rows(salt_build, salt_em_df)
    
  }
  
  prediction_df <- salt_build |> 
    mutate(model_id = config$inflow$forecast_inflow_model) |>
    mutate(site_id = config$location$site_id) |>
    mutate(reference_datetime = config$run_config$forecast_start_datetime) |>
    mutate(family = 'ensemble') |>
    mutate(variable = 'SALT') |>
    mutate(flow_type = 'inflow') |>
    mutate(flow_number = 1) |>
    rename(parameter = ensemble) |>
    select(model_id, site_id, reference_datetime, datetime, family, parameter, variable, prediction, flow_type, flow_number)
  
  return(prediction_df)
  
}
