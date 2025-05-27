#' Title
#'
#' @param targets link to the targets file
#' @param forecast_date reference_datetime
#' @param h horizon
#' @param model_id model_id
#' @param var variable
#' @param site site_id
#' @param depth depth
#'
#' @return
#' @export
#'
#' @examples
source('R/interpolate_targets.R')
generate_baseline_climatology <- function(targets = 'ALEX-targets-insitu.csv', 
                                          forecast_date, 
                                          h,
                                          model_id = 'climatology',
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
  
  variable_df <- data.frame(doy = seq(1,366, 1),
                            depth = depth,
                            variable = var,
                            site_id = site)
  
  # calculate the mean and standard deviation for each doy
  target_clim <- targets_df %>%
    filter(variable == var,
           depth == depth | is.na(depth),
           site_id == site,
           datetime < forecast_date) %>%
    mutate(doy = ifelse((yday(datetime) > 59 & lubridate::leap_year(datetime) != T), # DOY with leap year
                        yday(datetime) + 1,
                        yday(datetime))) |> 
    reframe(.by = c('doy', 'site_id', 'variable', 'depth'),
            clim_mean = mean(observation, na.rm = TRUE),
            clim_sd = sd(observation, na.rm = TRUE)) |> 
    full_join(variable_df, by = c('doy', 'site_id', 'variable', 'depth')) |>
    arrange(doy)
  
  if (nrow(target_clim) == 0) {
    message('No targets available. Check that the dates, depths, and sites exist in the target data frame')
    return(NULL)
  } else {
    # what dates do we want a forecast of?
    
    start_date <- as_date(forecast_date) + days(1)
    
    forecast_dates <- seq(start_date, as_date(start_date + days(h-1)), "1 day")
    forecast_doy <- yday(forecast_dates)
    
    # put in a table
    forecast_dates_df <- tibble(datetime = forecast_dates,
                                doy = forecast_doy)
    
    forecast <- target_clim %>%
      mutate(doy = as.integer(doy)) %>%
      filter(doy %in% forecast_doy) %>%
      full_join(forecast_dates_df, by = 'doy') %>%
      arrange(site_id, datetime)
    
    combined <- forecast %>%
      select(datetime, site_id, variable, clim_mean, clim_sd, depth) %>%
      rename(mean = clim_mean,
             sd = clim_sd) %>%
      group_by(site_id, variable) %>%
      mutate(mu = imputeTS::na_interpolation(x = mean),
             sigma = imputeTS::na_interpolation(x = sd)) |>
      
      # get in standard format
      pivot_longer(c("mu", "sigma"),names_to = "parameter", values_to = "prediction") |>
      mutate(family = "normal") |>
      mutate(reference_datetime = as_datetime(forecast_date),
             model_id = model_id) |>
      select(model_id, datetime, reference_datetime, site_id, variable, family, parameter, prediction, depth) |>
      as_tibble()
    
    return(combined)
  }
  
}

