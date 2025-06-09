#--------------------------------------#
## Project: ALEX-reforecasts
## Script purpose: generate a climatology null model
## Date: 2025-04-10
## Author: Freya Olsson
#--------------------------------------#

set.seed(100)
readRenviron("~/.Renviron") # MUST come first
library(tidyverse)
library(lubridate)
lake_directory <- here::here()
setwd(lake_directory)

source('R/generate_climatology.R')

starting_index <- 1


if (starting_index == 1) {
  config <- FLAREr::set_up_simulation(configure_run_file,lake_directory, config_set_name = config_set_name,
                                      clean_start = T) # start a clean run with spin up
} else {
  config <- FLAREr::set_up_simulation(configure_run_file,lake_directory, config_set_name = config_set_name,
                                      clean_start = F) # use restart
}


# Generate targets
if (!file.exists('targets/ALEX/ALEX-targets-insitu.csv')) {
  source(file.path('workflows/scenario_reforecasts/generate_targets_scenarios.R'))
  
} else {
  if (as_date(file.info('targets/ALEX/ALEX-targets-insitu.csv')$mtime) != Sys.Date()) {
    source(file.path('workflows/scenario_reforecasts/generate_targets_scenarios.R'))
  }
}



message("Successfully generated targets")


# Set up ------------------------------
# Reforecasts to run -----------------#
num_forecasts <- 92
days_between_forecasts <- 7
forecast_horizon <- 30
starting_date <- as_date("2023-07-01") # from the start of a water year
all_forecast_dates <- paste(seq.Date(starting_date, by = days_between_forecasts, length.out = num_forecasts), "00:00:00")
# ------------------------------------#

# Forecast set up
model_id <- 'climatology'
site_id <- 'ALEX'

# Run forecasts ------------------------
for (i in starting_index:length(all_forecast_dates)) { #
  
  ref_date <- all_forecast_dates[i]
  
  message("Forecast on ", ref_date)
  
  climatology_fc <- purrr::pmap(list(c('salt', 'temperature', 'depth'),
                                     c(0.5, 0.5, NA)),
                                ~generate_baseline_climatology(targets = 'ALEX-targets-insitu.csv', 
                                                               forecast_date = ref_date,
                                                               h = 30, 
                                                               model_id = 'climatology', site = 'ALEX',
                                                               var = ..1, depth = ..2)) |> 
    list_rbind() |> 
    mutate(reference_date = gsub(" 00:00:00", "", 
                                 ref_date))
  
  # clim_fc <- bind_rows(clim_fc, climatology_fc)
  # write forecasts!
  climatology_fc |> 
    mutate(datetime = as_datetime(datetime),
           reference_datetime = as_datetime(reference_datetime)) |> 
    group_by(site_id, model_id, reference_date) |>
    arrow::write_dataset(path = 'forecasts/parquet')
}


#----------------------------------------#

# Score forecasts! -----------------------
all_forecast_dates <- gsub(" 00:00:00", "", all_forecast_dates)

forecasts_df <- arrow::open_dataset('forecasts/parquet/') |>
  dplyr::filter(model_id == 'climatology',
                site_id == "ALEX",
                reference_date %in% all_forecast_dates) |>
  dplyr::collect() |> 
  mutate(variable_type = 'state')

source('R/generate_forecast_score_arrow.R')

targets_df <- read_csv("targets/ALEX/ALEX-targets-insitu.csv",show_col_types = FALSE)

for (i in 1:length(all_forecast_dates)) {
  
  to_score <- forecasts_df |> 
    dplyr::filter(reference_date == all_forecast_dates[i])
  
  scoring <- generate_forecast_score_arrow(targets_df = targets_df,
                                           forecast_df = to_score, 
                                           use_s3 = FALSE,
                                           bucket = NULL,
                                           endpoint = NULL,
                                           local_directory = 'scores',
                                           variable_types = c("state"))
  message(i , '/', length(all_forecast_dates))
  
}

