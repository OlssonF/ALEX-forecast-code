# Generate some simple historic flows based on the targets
source("R/interpolate_targets.R")

# Inflows
hist_interp_inflow <- interpolate_targets(targets = 'ALEX-targets-inflow.csv', 
                                         lake_directory = lake_directory,
                                         targets_dir = 'targets', 
                                         site_id = 'ALEX', 
                                         variables = c('FLOW', 'SALT', 'TEMP'),
                                         depth = F, 
                                         method = 'linear')

# Write the interpolated data as the historal file
arrow::write_dataset(hist_interp_inflow,
                     'drivers/inflow/historical/historical_interp/site_id=ALEX/')

# generate a simple "forecast" that has ensemble members
forecast_date <- config$run_config$forecast_start_datetime
future_inflow <- hist_interp_inflow |> 
  filter(datetime > forecast_date,
         datetime <= as_date(forecast_date) + config$run_config$forecast_horizon) |> 
  reframe(prediction = rnorm(10, mean = observation, sd = 1), 
          parameter = 1:10,
          .by = c(site_id, datetime, variable)) |> 
  mutate(reference_datetime = as_date(forecast_date))

arrow::write_dataset(future_inflow,
                     'drivers/inflow/future/historical_interp/',
                     partitioning = c('reference_datetime', 'site_id'))

#==========================================#

# outflows
hist_interp_outflow <- interpolate_targets(targets = 'ALEX-targets-outflow.csv', 
                                          lake_directory = lake_directory,
                                          targets_dir = 'targets', 
                                          site_id = 'ALEX', 
                                          variables = c('FLOW', 'SALT', 'TEMP'),
                                          depth = F, 
                                          method = 'linear')

# Write the interpolated data as the historal file
arrow::write_dataset(hist_interp_inflow,
                     'drivers/outflow/historical/historical_interp/site_id=ALEX/')

# generate a simple "forecast" that has ensemble members
future_outflow <- hist_interp_outflow |> 
  filter(datetime > forecast_date,
         datetime <= as_date(forecast_date) + config$run_config$forecast_horizon) |> 
  reframe(prediction = rnorm(10, mean = observation, sd = 1), 
          parameter = 1:10,
          .by = c(site_id, datetime, variable)) |> 
  mutate(reference_datetime = as_date(forecast_date))

arrow::write_dataset(future_outflow,
                     'drivers/outflow/future/historical_interp/',
                     partitioning = c('reference_datetime', 'site_id'))
