
source('R/interpolate_targets.R')
library(tsibble)

#' Generate baseline persistence forecast
#'
#' @param targets name of the targets file 
#' @param forecast_date the reference_date of the forecast
#' @param h horizon
#' @param ensemble_size number of ensemble members
#' @param model_id name of model
#' @param var variable
#' @param site site_id
#' @param depth depth
#'
#' @returns dataframe
#' @export
#'
#' @examples
generate_baseline_persistenceRW <- function(targets = 'ALEX-targets-insitu.csv', 
                                            forecast_date, 
                                            h,
                                            ensemble_size,
                                            model_id = 'persistenceRW',
                                            var,
                                            site = 'ALEX',
                                            depth) {
  
  
  targets_df <- interpolate_targets(targets = targets, 
                                    lake_directory = lake_directory,
                                    targets_dir = 'targets', 
                                    site_id = site, 
                                    variables = var,
                                    groups = 'depth',
                                    method = 'linear') 
  
  
  # make into a tsibble
  
  targets_ts <- targets_df |>
    mutate(datetime = lubridate::as_date(datetime)) |>
    filter(variable %in% var,
           site_id %in% site,
           depth %in% depth,
           datetime < forecast_date) |>
    as_tsibble(key = c('variable', 'site_id', 'depth'), index = 'datetime') |>
    # add NA values up to today (index)
    fill_gaps(.end = forecast_date)
  
  
  # Work out when the forecast should start
  forecast_starts <- targets_df %>%
    dplyr::filter(!is.na(observation) & site_id == site & variable == var & datetime < forecast_date) %>%
    # Start the day after the most recent non-NA value
    dplyr::summarise(start_date = as_date(max(datetime)) + lubridate::days(1)) %>% # Date
    dplyr::mutate(h = (as_date(forecast_date) - start_date) + h) %>% # Horizon value
    dplyr::ungroup()
  
  # filter the targets data set to the site_var pair
  targets_use <- targets_ts |>
    dplyr::filter(datetime < forecast_starts$start_date)
  
  if (var %in% c('salt', 'depth')) { # use log for salt and depth
    RW_model <- targets_use %>%
      fabletools::model(RW = fable::RW(log(observation)))
  } else {
    RW_model <- targets_use %>%
      fabletools::model(RW = fable::RW(observation))
  }
 
  
  forecast <- RW_model %>%
    fabletools::generate(h = as.numeric(forecast_starts$h),
                         bootstrap = T,
                         times = ensemble_size) |>
    rename(parameter = .rep,
           prediction = .sim) |>
    mutate(model_id = model_id,
           family = 'ensemble',
           reference_datetime = forecast_date)  |>
    select(any_of(c("model_id", "datetime", "reference_datetime","site_id", "variable", "family",
                    "parameter", "prediction","depth" )))|>
    select(-any_of('.model'))|>
    filter(datetime > reference_datetime)|>
    ungroup() |>
    as_tibble()
  
  return(forecast)
}