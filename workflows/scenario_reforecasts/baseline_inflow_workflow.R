# Combined flow drivers workflow

# Historical ----------------------------------
# Generate some simple historic flows based on the targets
source("R/interpolate_targets.R")
source('R/inflow_salt_xgboost.R')
source('R/inflow_temperature_xgboost.R')
source('R/inflow_flow_process.R')

site_id <- 'ALEX'
message('----Making historical inflows----')
# ==== Historical interpolation inflows ====
hist_interp_inflow <- interpolate_targets(targets = 'ALEX-targets-inflow.csv', 
                                          lake_directory = lake_directory,
                                          targets_dir = 'targets', 
                                          site_id = site_id, 
                                          variables = c('FLOW', 'SALT', 'TEMP'),
                                          groups = 'inflow_name',
                                          method = 'linear') |> 
  mutate(flow_number = ifelse(inflow_name == 'murray', 1, ifelse(inflow_name == 'finnis', 2, NA)), 
         parameter = 1) |> 
  rename(prediction = observation) |> 
  dplyr::select(any_of(c("site_id", "flow_number", "datetime", "variable", "prediction", "parameter"))) |> 
  # use only a single inflow
  dplyr::filter(flow_number == 1)

# These observations for flow are from the SA border - need to apply the loss and travel time model!
hist_interp_upstream_flow <- hist_interp_inflow |> 
  dplyr::filter(variable == 'FLOW') |> 
  # make sure it's in MLd!
  # mutate(prediction = prediction * 86.4) |> 
  rename(flow = prediction)

# Make sure the units for the loss data are the same as for the prediction
L_mod <- model_losses(model_dat = 'R/helper_data/modelled_losses.csv',
                      obs_unc = 0,
                      # data are losses in GL/m at different rates of entitlement flow (GL/d)
                      formula_use = "y ~ x + group", 
                      y = 'loss', x = 'flow', group = 'month')

TT_mod <- model_traveltime(model_dat = 'R/helper_data/travel_times.csv',
                           obs_unc = 0,
                           # data are travel time (days) at different rates of  flow (ML/d)
                           formula_use = 'y ~ poly(x, 3)',
                           y = 'travel_time', x = 'flow')


hist_interp_W_flow <- predict_downstream(data = hist_interp_upstream_flow, 
                                         forecast_dates = 'historical',
                                         upstream_col = 'flow',
                                         L_mod = L_mod, loss_unc = T,
                                         TT_mod = TT_mod, tt_unc = T) |> 
  mutate(prediction = prediction / 86.4,
         variable = 'FLOW', 
         site_id = site_id, 
         flow_number = 1, 
         parameter = 1) # convert from ML/d to m3/s
# combine with WQ vars

hist_interp_inflow <- dplyr::filter(hist_interp_inflow, variable != 'FLOW') |> 
  full_join(hist_interp_W_flow, 
            by = join_by(site_id, flow_number, datetime, variable, prediction, parameter))
# Write the interpolated data as the historical file
arrow::write_dataset(hist_interp_inflow,
                     glue::glue(config$flows$local_inflow_directory, "/", config$flows$historical_inflow_model))

# ==== Historical interpolation outflows ====
hist_interp_outflow <- interpolate_targets(targets = 'ALEX-targets-outflow.csv',
                                           lake_directory = lake_directory,
                                           targets_dir = 'targets',
                                           site_id = 'ALEX',
                                           variables = c('FLOW', 'SALT', 'TEMP'),
                                           groups = NULL,
                                           method = 'linear') |>
  mutate(flow_number = 1,
         parameter = 1) |>
  rename(prediction = observation)


# Write the interpolated data as the historal file
arrow::write_dataset(hist_interp_outflow,
                     glue::glue(config$flows$local_outflow_directory, "/", config$flows$historical_outflow_model))


# Future inflows =======
message('----Making future inflows----')

model_id <- 'combined_inflows'
site_id <- 'ALEX'
reference_date <- as_date(config$run_config$forecast_start_datetime)
horizon <- config$run_config$forecast_horizon
ens_members <- config$da_setup$ensemble_size

message('---Salt inflow forecast---')
salt_fc <- generate_salt_inflow_fc(config) 
message('---Temperature inflow forecast---')
temp_fc <- generate_temp_inflow_fc(config) 


# Make sure the units for the loss data are the same as for the prediction
message('---Flow inflow forecast---')
L_mod <- model_losses(model_dat = 'R/helper_data/modelled_losses.csv',
                      obs_unc = 0.05,
                      # data are losses in GL/m at different rates of entitlement flow (GL/d)
                      formula_use = "y ~ x + group", 
                      y = 'loss', x = 'flow', group = 'month')

TT_mod <- model_traveltime(model_dat = 'R/helper_data/travel_times.csv',
                           obs_unc = 0.05,
                           # data are travel time (days) at different rates of  flow (ML/d)
                           formula_use = 'y ~ poly(x, 3)',
                           y = 'travel_time', x = 'flow')


flow_fc <- generate_flow_inflow_fc(config = config, 
                                   upstream_unit = 'MLd',
                                   # lag_t = 9:14, # this is the range of lags that are applied in the model 
                                   loss_unc = T, # is there uncertainty in the loss model? if TRUE, applies sd in L_mod residuals
                                   tt_unc = T, # is there uncertainty in the travel time model? if TRUE, applies sd in TT_mod residuals
                                   n_members = config$da_setup$ensemble_size, # same as the salt and temp forecasts
                                   upstream_location = 'QSA',
                                   TT_mod = TT_mod,
                                   L_mod = L_mod) |> 
  mutate(#convert from ML/d to m3/s
         prediction = prediction/86.4) 
# make sure it has the same number of parameter values as the other forecasts!!
# reframe(parameter=unique(salt_fc$parameter), .by = everything())


inflow_fc <- bind_rows(flow_fc, temp_fc, salt_fc) |> 
  mutate(site_id = site_id,
         model_id = 'combined_inflow') |> 
  rename(reference_datetime = reference_date)


arrow::write_dataset(inflow_fc,
                     glue::glue(config$flows$local_inflow_directory,
                                "/", config$flows$future_inflow_model))


# ==== Future outflow persistenceRW =====
# fit the model only for the last month
message('----Making historical outflows----')
# Get the observations and interpolate
hist_interp_outflow <- interpolate_targets(targets = 'ALEX-targets-outflow.csv',
                                           lake_directory = lake_directory,
                                           targets_dir = 'targets',
                                           site_id = 'ALEX',
                                           variables = c('FLOW', 'SALT', 'TEMP'),
                                           groups = NULL,
                                           method = 'linear')

# When was the last observation? When to start the forecast
forecast_info <- hist_interp_outflow  |> 
  dplyr::filter(datetime < reference_date) |> # remove observations after reference_date
  summarise(last_obs = max(datetime),
            .by = c(site_id, variable)) |> 
  mutate(horizon = as.numeric(as_date(reference_date) - as_date(last_obs) + horizon)) |> 
  # what is the total horizon including missing days of observations up to the reference_date
  mutate(start_training = last_obs - days(60))

message('----Making future persistence outflow----')
future_outflow_RW <- hist_interp_outflow |> 
  # filter so we only train on observations within the last month of the last observation
  dplyr::filter(datetime < reference_date,
         datetime >= forecast_info$start_training) |>
  
  # Make the obs into a tsibble
  as_tsibble(key = c(site_id, variable), index = datetime) |> 
  
  # Fit the model
  fabletools::model(RW = fable::RW(box_cox(observation, 0.3))) |>  
  
  # generate forecast of specific horizon with 31 parameters
  fabletools::generate(h = 30, times = config$da_setup$ensemble_size, bootstrap = T) |> 
  as_tibble() |> 
  
  rename(parameter = .rep,
         prediction = .sim) |> 
  mutate(reference_datetime = as_date(reference_date),
         model_id = "persistenceRW",
         parameter = as.numeric(parameter) - 1,
         flow_number = 1,
         prediction = ifelse(prediction < 0, 0, prediction)) |> 
  arrange(datetime, parameter) |> 
  select(-.model)

arrow::write_dataset(future_outflow_RW,
                     glue::glue(config$flows$local_outflow_directory,
                                "/", config$flows$future_outflow_model))
