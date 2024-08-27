# Generate some simple historic flows based on the targets
source("R/interpolate_targets.R")

# Inflows
hist_interp_inflow <- interpolate_targets(targets = 'ALEX-targets-inflow.csv', 
                                          lake_directory = lake_directory,
                                          targets_dir = 'targets', 
                                          site_id = 'ALEX', 
                                          variables = c('FLOW', 'SALT', 'TEMP'),
                                          groups = 'inflow_name',
                                          method = 'spline') |> 
  mutate(flow_number = ifelse(inflow_name == 'murray', 1, ifelse(inflow_name == 'finnis', 2, NA)), 
         parameter = 1) |> 
  rename(prediction = observation) |> 
  select(site_id, flow_number, datetime, variable, prediction, parameter)

# Write the interpolated data as the historical file
arrow::write_dataset(hist_interp_inflow,
                     file.path(config$flows$local_inflow_directory, config$flows$historical_inflow_model, 
                               paste0("site_id=",config$location$site_id)))

# generate a simple "forecast" that has ensemble members
forecast_date <- config$run_config$forecast_start_datetime
future_inflow <- hist_interp_inflow |> 
  filter(datetime >= as_datetime(forecast_date),
         datetime <= as_date(forecast_date) + config$run_config$forecast_horizon) |> 
  # reframe(prediction = rnorm(10, mean = prediction, sd = 1), 
  #         parameter = 1:10,
  #         .by = c(site_id, datetime, variable, flow_number)) |> 
  mutate(reference_datetime = as_date(forecast_date),
         prediction = ifelse(prediction <0, 0, prediction))

arrow::write_dataset(future_inflow,
                     file.path(config$flows$local_inflow_directory, config$flows$future_inflow_model),
                     partitioning = c('reference_datetime', 'site_id'))

#==========================================#

# outflows
hist_interp_outflow <- interpolate_targets(targets = 'ALEX-targets-outflow.csv', 
                                           lake_directory = lake_directory,
                                           targets_dir = 'targets', 
                                           site_id = 'ALEX', 
                                           variables = c('FLOW', 'SALT', 'TEMP'),
                                           groups = 'inflow_name',
                                           method = 'spline') |> 
  mutate(flow_number = 1, 
         parameter = 1) |> 
  rename(prediction = observation)


# Write the interpolated data as the historal file
arrow::write_dataset(hist_interp_outflow,
                     file.path(config$flows$local_outflow_directory, config$flows$historical_outflow_model, 
                               paste0("site_id=",config$location$site_id)))

# generate a simple "forecast" that has ensemble members
future_outflow <- hist_interp_outflow |>
  filter(datetime >= as_datetime(forecast_date),
         datetime <= as_date(forecast_date) + config$run_config$forecast_horizon) |>
  # reframe(prediction = rnorm(10, mean = prediction, sd = 1),
  #         parameter = 1:10,
  #         .by = c(site_id, datetime, variable, flow_number)) |>
  mutate(reference_datetime = as_date(forecast_date),
         prediction = ifelse(prediction <0, 0, prediction))

arrow::write_dataset(future_outflow,
                     file.path(config$flows$local_outflow_directory, config$flows$future_outflow_model),
                     partitioning = c('reference_datetime', 'site_id'))

# future_barrage_closed <- hist_interp_outflow |> 
#   filter(datetime >= as_datetime(forecast_date),
#          datetime <= as_date(forecast_date) + config$run_config$forecast_horizon) |> 
#   # reframe(prediction = rnorm(10, mean = prediction, sd = 1), 
#   #         parameter = 1:10,
#   #         .by = c(site_id, datetime, variable, flow_number)) |> 
#   mutate(reference_datetime = as_date(forecast_date),
#          prediction = 0)
# 
# arrow::write_dataset(future_outflow,
#                      file.path(config$flows$local_outflow_directory, config$flows$future_outflow_model),
#                      partitioning = c('reference_datetime', 'site_id'))