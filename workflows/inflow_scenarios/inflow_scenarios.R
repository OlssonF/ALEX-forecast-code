# combine a "baseline" forecast with an entitlement flow/ eflow forecast
# To avoid jumps in flow the two forecasts (baseline + scenario) are gradually merged together
# based on a change in weighting from one to the other

library(tidyverse)
source('R/scenario_inflow_fc.R')

# 
# config <- list(run_config = list(forecast_start_datetime = '2024-01-04 00:00:00',
#                                  forecast_horizon = 30,
#                                  end_datetime = NA),
#                location = list(site_id = "ALEX"),
#                flows = list(local_inflow_directory = "drivers/inflow",
#                             future_inflow_model = "future/model_id=combined_inflow/reference_date={reference_date}/site_id={site_id}"))
# Combined flow drivers workflow

# Historical ----------------------------------
# Generate some simple historic flows based on the targets
source("R/interpolate_targets.R")

site_id <- 'ALEX'
message('Making historical flows')
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
  select(site_id, flow_number, datetime, variable, prediction, parameter) |> 
  # use only a single inflow
  filter(flow_number == 1)

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
message('running inflow models')

site_id <- 'ALEX'
reference_date <- as_date(config$run_config$forecast_start_datetime)
horizon <- config$run_config$forecast_horizon
lag_t <- 14

message("generating baseline salt forecast")
source('R/inflow_salt_xgboost_temporary.R')
salt_fc <- generate_salt_inflow_fc(config)

message("generating baseline temp forecast")
source('R/inflow_temperature_xgboost_temporary.R')
temp_fc <- generate_temp_inflow_fc(config)

if (!is.na(config$run_config$forecast_horizon)) {
  forecast_dates <- seq.Date(as_date(config$run_config$forecast_start_datetime), 
                             length.out = config$run_config$forecast_horizon, 
                             by = 'day')
} else if (!is.na(config$run_config$end_datetime)) {
  forecast_dates <- seq.Date(as_date(config$run_config$forecast_start_datetime), 
                             as_date(config$run_config$end_datetime),
                             by = 'day')
} else {
  stop("needs a horizon or an end date")
}

# Future inflow scenarios
# Step 1: generate the "baseline" forecast -----------------------
message("generating baseline flow forecast")
L_mod <- model_losses(model_dat = 'R/helper_data/modelled_losses_DEW.csv', 
                      # data are losses in GL/m at different rates of entitlement flow (GL/d)
                      formula_use = "x ~ y + group", 
                      x = 'loss', y = 'flow', group = 'month')

# this is the same function that is used in the baseline workflow, returned in ML/d
flow_fc <- generate_flow_inflow_fc(config = config, 
                                   upstream_unit = 'MLd',
                                   lag_t = lag_t, 
                                   upstream_location = 'QSA',
                                   L_mod = L_mod, 
                                   model_id = 'baseline') |>
  filter(datetime %in% forecast_dates) |>
  select(datetime, prediction) |> 
  mutate(model_id = 'baseline')



# Step 2: generate the two scenarios -----------------------------
message("generating scenario flow forecast")
# Entitlement flow only - the min flow into SA
ent_QSA <- generate_entflow_inflow_fc(config, 
                                      entitlement_dat = 'R/helper_data/entitlement_flow.csv', 
                                      lag_t = lag_t)|> 
  rename(flow = ent_MLd)

eflow_QSA <-  generate_eflow_inflow_fc(config, 
                                       eflow_dat  = 'R/helper_data/eflow.csv', 
                                       lag_t = lag_t) |> 
  # add to the entitlement flow
  mutate(eflow_MLd = eflow_MLd + ent_QSA$flow) |> 
  rename(flow = eflow_MLd)  

## Apply loss model to SA border 
message('applying loss model to predict Wellington')
ent_fc <- predict_downstream(lag_t = 14, 
                             data = ent_QSA, 
                             upstream_col = 'flow',
                             L_mod = L_mod) |>
  filter(datetime %in% forecast_dates) |>
  select(datetime, prediction) |> 
  mutate(model_id = 'entitlement')

eflow_fc <- predict_downstream(lag_t = 14, 
                               data = eflow_QSA, 
                               upstream_col = 'flow',
                               L_mod = L_mod) |>
  filter(datetime %in% forecast_dates)|>
  select(datetime, prediction) |> 
  mutate(model_id = 'eflow')


# Step 3: generate two new forecasts (eflows + baseline; ent_flow + baseline) -----
# need to make sure there are enough weights in case of additional missing values
message('combining forecasts')
flow_fc_scenario <- full_join(flow_fc, ent_fc, by = join_by(datetime, prediction, model_id)) |>
  full_join(eflow_fc, by = join_by(datetime, prediction, model_id)) |> 
  pivot_wider(names_from = model_id, values_from = prediction) |> 
  mutate(weight1 = seq(from = 0, to = 1, length.out = length(forecast_dates)),
         weight2 = 1-weight1,
         base_ent = ((entitlement * weight1) + (baseline * weight2)),
         base_eflow = ((eflow * weight1) + (baseline * weight2)))


flow_fc_scenario_faster <- full_join(flow_fc, ent_fc, by = join_by(datetime, prediction, model_id)) |>
  full_join(eflow_fc, by = join_by(datetime, prediction, model_id)) |> 
  pivot_wider(names_from = model_id, values_from = prediction) |> 
  mutate(weight1 = c(seq(from = 0, to = 1, length.out = length(forecast_dates)/2),
                     rep(1, length(forecast_dates)/2)),
         weight2 = 1-weight1,
         base_ent = ((entitlement * weight1) + (baseline * weight2)),
         base_eflow = ((eflow * weight1) + (baseline * weight2)))


# Step 4: write or save for use in FLARE ---------
message('writing scenario forecasts')
## Entitlement flow scenario
scenario_model1 <- str_replace(config$flows$future_inflow_model, pattern = 'combined_inflow', 'entitlement_inflow')

flow_fc_scenario |>
  select(datetime, base_ent) |> 
  rename(prediction = base_ent) |> 
  # make sure it has the same number of parameter values as the WQ forecasts!!
  reframe(parameter=seq(0,30,1), .by = everything()) |>
  mutate(reference_date = as_date(reference_date),
         datetime = as_date(datetime),
         variable = 'FLOW',
         flow_number = 1) |>
  select(any_of(c('datetime', 'prediction', 'reference_date', 'model_id',
                  'variable', 'flow_number', 'parameter'))) |> 
  bind_rows(temp_fc, salt_fc) |> 
  mutate(site_id = site_id,
         model_id = 'entitlement_inflow') |> 
  rename(reference_datetime = reference_date) |> 
  arrow::write_dataset(glue::glue(config$flows$local_inflow_directory,
                                  "/", scenario_model1))

## Eflow scenario
scenario_model2 <- str_replace(config$flows$future_inflow_model, pattern = 'combined_inflow', 'eflow_inflow')

flow_fc_scenario |>
  select(datetime, base_eflow) |> 
  rename(prediction = base_eflow) |> 
  # make sure it has the same number of parameter values as the WQ forecasts!!
  reframe(parameter=seq(0,30,1), .by = everything()) |>
  mutate(reference_date = as_date(reference_date),
         datetime = as_date(datetime),
         variable = 'FLOW',
         flow_number = 1) |>
  select(any_of(c('datetime', 'prediction', 'reference_date', 'model_id',
                  'variable', 'flow_number', 'parameter'))) |> 
  bind_rows(temp_fc, salt_fc) |> 
  mutate(site_id = site_id,
         model_id = 'eflow_inflow') |> 
  rename(reference_datetime = reference_date) |> 
  arrow::write_dataset(glue::glue(config$flows$local_inflow_directory,
                                  "/", scenario_model2))
# ==== Future outflow persistenceRW =====
# fit the model only for the last month
message('Making peristence outflow forecast')
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
  filter(datetime < reference_date) |> # remove observations after reference_date
  summarise(last_obs = max(datetime),
            .by = c(site_id, variable)) |> 
  mutate(horizon = as.numeric(as_date(reference_date) - as_date(last_obs) + horizon)) |> 
  # what is the total horizon including missing days of observations up to the reference_date
  mutate(start_training = last_obs - days(60))


future_outflow_RW <- hist_interp_outflow |> 
  # filter so we only train on observations within the last month of the last observation
  filter(datetime < reference_date,
         datetime >= forecast_info$start_training) |>
  
  # Make the obs into a tsibble
  as_tsibble(key = c(site_id, variable), index = datetime) |> 
  
  # Fit the model
  fabletools::model(RW = fable::RW(box_cox(observation, 0.3))) |>  
  
  # generate forecast of specific horizon with 31 parameters
  fabletools::generate(h = 30, times = 31, bootstrap = T) |> 
  as_tibble() |> 
  
  rename(parameter = .rep,
         prediction = .sim) |> 
  mutate(reference_datetime = as_date(reference_date),
         model_id = "persistenceRW",
         flow_number = 1,
         prediction = ifelse(prediction < 0, 0, prediction)) |> 
  select(-.model)

arrow::write_dataset(future_outflow_RW,
                     glue::glue(config$flows$local_outflow_directory,
                                "/", config$flows$future_outflow_model))