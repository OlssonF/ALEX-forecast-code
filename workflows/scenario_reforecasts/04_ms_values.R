#--------------------------------------#
## Project: ALEX-reforecasts
## Script purpose: recreate tables and values that appear in the manuscript
## Date: 2025-04-24
## Author:
#--------------------------------------#

# package loading ---------------------------
library(tidyverse)
library(arrow)

# Set up -------------------------------------------------------
## read in data ------------------------------------------------
model_ids <- c('reference' = 'glm_flare_v3_crest',
               'raise' = 'glm_flare_v3_crest_up_0.1',
               'lower' = 'glm_flare_v3_crest_down_0.1')
scenarios <- names(model_ids)
site_id <- 'ALEX'
forecast_horizon <- 30

starting_date <- as_date("2023-07-01") # from the start of a water year
ref_dates <- seq.Date(starting_date, by = 7, length.out = 92)

forecasts <- open_dataset('forecasts/parquet/site_id=ALEX/') |> 
  filter(model_id %in% model_ids,
         reference_date %in% ref_dates) |> 
  collect() |> 
  mutate(scenario = ifelse(model_id == 'glm_flare_v3_crest',
                           'reference',
                           ifelse(model_id == "glm_flare_v3_crest_down_0.1",
                                  'lower barrages', "raise barrages")))

scores <- open_dataset('scores/site_id=ALEX/') |> 
  filter(model_id %in% c('glm_flare_v3_crest', 'climatology', "persistenceRW"),
         reference_date %in% ref_dates) |> 
  collect() 

insitu_obs <- read_csv('targets/ALEX/ALEX-targets-insitu.csv') |> 
  mutate(variable = ifelse(variable == 'salt', 'insitu salinity', 
                           ifelse(variable == 'temperature', 'insitu temperature', variable)),
         observation = ifelse(variable == 'depth', observation - 5.3, observation))

inflow_obs <- read_csv('targets/ALEX/ALEX-targets-inflow.csv') |>
  filter(inflow_name == 'murray', variable == 'FLOW') |> 
  mutate(variable = ifelse(variable == 'FLOW', 'inflow', variable))

outflow_obs <- read_csv('targets/ALEX/ALEX-targets-outflow.csv') |> 
  filter(variable == 'FLOW') |> 
  mutate(variable = ifelse(variable == 'FLOW', 'barrage outflow', 
                           ifelse(variable == 'salt', 'outflow salt', variable)))


# Results values ---------------------------
## Observations ---------------------------
bind_rows(insitu_obs, inflow_obs, outflow_obs)|> 
  filter(between(datetime, 
                 as_datetime('2023-07-01'),
                 as_datetime('2025-05-01'))) |> 
  select(-depth) |> 
  pivot_wider(values_from = observation, names_from = variable) |> 
  mutate(`barrage outflow` = `barrage outflow`*86.4,
         `salt export` = (`insitu salinity`*`barrage outflow`),
         `electrical conductivity` = gsw::gsw_C_from_SP(SP = `insitu salinity`, t = `insitu temperature`, p = 0)*1000) |> 
  pivot_longer(depth:`electrical conductivity`, names_to = 'variable', values_to = 'observation') |> 
  group_by(variable) |> 
  summarise(mean = mean(observation, na.rm = T),
            median = median(observation, na.rm = T),
            sd = sd(observation, na.rm = T),
            min = min(observation, na.rm = T),
            max = max(observation, na.rm = T))

# water year inflow calcs
inflow_obs |> 
  filter(between(datetime, 
                 as_datetime('2023-07-01'),
                 as_datetime('2025-05-01'))) |>
  mutate(water_year = ifelse(between(datetime, as_date('2023-07-01'), as_date('2024-07-01')), 2023, 2024)) |> 
  reframe(.by = water_year, 
          median = median(observation, na.rm = T),
          mean = mean(observation, na.rm = T))

# July-Dec inflow calcs
inflow_obs |> 
  filter(between(datetime, 
                 as_datetime('2023-07-01'),
                 as_datetime('2025-05-01'))) |>
  mutate(year = year(datetime), 
         month = month(datetime)) |> 
  filter(month %in% 7:12, 
         year %in% 2023:2024) |> 
  reframe(.by = year, 
          total = sum(observation)/1000) |> # report GL 
  pivot_wider(names_from = year,
              values_from = total, names_prefix = 'y_') |> 
  mutate(diff = y_2023 - y_2024,
         percent_change = diff/y_2024)
  
## total salt and water export ------------------
bind_rows(insitu_obs, outflow_obs)|> 
  filter(between(datetime, 
                 as_datetime('2023-07-01'),
                 as_datetime('2025-05-01'))) |> 
  select(-depth) |> 
  pivot_wider(values_from = observation, names_from = variable) |> 
  mutate(`barrage outflow` = `barrage outflow` * 86.4,
         `salt export` = (`insitu salinity`*`barrage outflow`)) |> 
  pivot_longer(depth:`salt export`, names_to = 'variable', values_to = 'observation') |> 
  filter(variable %in% c('barrage outflow', 'salt export')) |> 
  group_by(variable) |> 
  summarise(sum = sum(observation, na.rm = T))

# water year salt and water export
# water year inflow calcs
bind_rows(insitu_obs, outflow_obs)|> 
  filter(between(datetime, 
                 as_datetime('2023-07-01'),
                 as_datetime('2025-05-01'))) |> 
  select(-depth) |> 
  pivot_wider(values_from = observation, names_from = variable) |> 
  mutate(`barrage outflow` = `barrage outflow` * 86.4,
         `salt export` = (`insitu salinity`*`barrage outflow`)) |> 
  pivot_longer(depth:`salt export`, names_to = 'variable', values_to = 'observation') |> 
  filter(variable %in% c('barrage outflow', 'salt export')) |> 
  mutate(water_year = ifelse(between(datetime, as_date('2023-07-01'), as_date('2024-07-01')), 2023, 2024)) |> 
  reframe(.by = c(variable, water_year), 
          sum = sum(observation, na.rm = T))

# CRPS summaries
eval_vars <- c('temperature', 'salt', 'depth')
eval_depths <- 0.5 # plus NA for depth variable

scores |>
  filter(variable %in% eval_vars,
         depth %in% eval_depths | is.na(depth),
         horizon > 0) |> 
  mutate(sq_error = (median - observation)^2) |> 
  reframe(.by = all_of(c('horizon', 'variable', 'depth', 'model_id')),
          median_crps = median(crps, na.rm = T),
          rmse = sqrt(mean(sq_error, na.rm = T))) |>
  filter(model_id == 'glm_flare_v3_crest', 
         horizon %in% c(1,7,14,30))

## High vs low flow evaluation ---------------------
inflow_percentiles <- forecasts |> ungroup() |> 
  filter(scenario == 'reference', 
         variable == 'inflow',
         datetime > as_date(reference_date)) |> 
  reframe(.by = reference_date,
          mean_inflow = mean(prediction)) |> 
  reframe(high_flow = quantile(mean_inflow, 0.75),
          low_flow = quantile(mean_inflow, 0.25))

# bin each reference_date
flow_cats <- forecasts |> ungroup() |> 
  filter(scenario == 'reference', 
         variable == 'inflow',
         datetime > as_date(reference_date)) |> 
  reframe(.by = reference_date,
          mean_inflow = mean(prediction)) |> 
  mutate(flow_category = ifelse(mean_inflow >= inflow_percentiles$high_flow, 'high_flow', 
                                ifelse(mean_inflow <= inflow_percentiles$low_flow, 'low_flow',
                                       'normal_flow'))) |> 
  select(-mean_inflow)

scores |>
  filter(variable %in% eval_vars,
         depth %in% eval_depths | is.na(depth),
         horizon > 0) |> 
  mutate(sq_error = (median - observation)^2) |> 
  full_join(flow_cats, by = 'reference_date') |> 
  reframe(.by = all_of(c('horizon', 'variable', 'depth', 'model_id', 'flow_category')),
          median_crps = median(crps, na.rm = T),
          rmse = sqrt(mean(sq_error, na.rm = T))) |>
  filter(model_id == 'glm_flare_v3_crest', 
         horizon %in% c(1,7,14,30),
         flow_category == 'high_flow')
  
# difference between scenarios ----------------------
## salt export
saltexport_fc |> 
  pivot_wider(values_from = prediction, 
              names_from = variable) |> 
  mutate(overflow_salt_load = (overflow_salt*overflow_flow)/1000) |> # units of salt is ppt (equal to kg/m3), units of discharge m3/day, divide by 1000 = tonnes
  reframe(.by = c(model_id, scenario, reference_date, parameter),
          total_salt_load = sum(overflow_salt_load)) |> 
  reframe(.by = c(model_id, scenario, reference_date),
          median = median(total_salt_load, na.rm = T)) |> 
  reframe(.by = scenario,
          average = mean(median)) |> 
  pivot_wider(names_from = scenario, values_from = average) |> 
  summarise(lower_perc = (`lower barrages` - reference) /reference *100,
            raise_perc = (`raise barrages` - reference) /reference *100,
            lower_val = (`lower barrages` - reference),
            raise_val = (`raise barrages` - reference))

# change in depth
depth_fc |> 
  mutate(horizon = as.numeric(as_date(datetime) - as_date(reference_date))) |> 
  filter(horizon %in% c(1,30)) |> 
  select(reference_date, parameter, scenario, model_id, horizon, prediction) |> 
  pivot_wider(names_from = horizon,
              names_prefix = "h_",
              values_from = prediction) |> 
  mutate(height_change = h_30 - h_1) |> 
  reframe(.by = c(model_id, scenario, reference_date),
          median = median(height_change, na.rm = T),
          quantile95 = quantile(height_change, 0.95, na.rm = T),
          quantile5 = quantile(height_change, 0.05, na.rm = T)) |> 
  reframe(.by = scenario,
          average = mean(median),
          sd = sd(median))

# inflow effect vs scenario effect
bind_rows(flow_extreme_fc  |> 
            select(reference_date, datetime, variable, prediction, scenario, flow_extreme) |> 
            pivot_wider(names_from = scenario, values_from = prediction) |> 
            mutate(diff = abs(`lower barrages` - `raise barrages`),
                   group = flow_extreme) |> 
            reframe(.by = c(reference_date, group, variable), 
                    median_diff = median(diff)),
          
          flow_extreme_fc  |> 
            select(reference_date, datetime, variable, prediction, scenario, flow_extreme) |> 
            pivot_wider(names_from = flow_extreme, values_from = prediction) |> 
            mutate(diff = abs(min - max), 
                   group = scenario) |> 
            reframe(.by = c(reference_date, group, variable), 
                    median_diff = median(diff))) |> 
  mutate(group = factor(group, levels =  c('min', 'max', 'lower barrages', 'raise barrages')),
         effect = ifelse(group %in% c('min', 'max'), 'scenario effect', 'inflow effect'),
         combined_group = factor(paste(effect, group, sep = ': '),
                                 levels = c('scenario effect: min', 'scenario effect: max',
                                            'inflow effect: lower barrages', 'inflow effect: raise barrages'),
                                 labels = c('scenario effect: min inflow', 'scenario effect: max inflow',
                                            'inflow effect: lower barrages', 'inflow effect: raise barrages'))) |> 
  group_by(variable, combined_group) |> 
  summarise(mean = mean(median_diff),
            sd = sd(median_diff))
