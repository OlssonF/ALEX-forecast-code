#--------------------------------------#
## Project: ALEX-reforecasts
## Script purpose: Evaluation of in-situ temperature and salinity forecasts and overflow_flow during the DA period
## Date: 2025-04-08
## Author: Freya Olsson
#--------------------------------------#
source('R/generate_forecast_score_arrow.R')
# Reforecasts to score -----------------#
num_forecasts <- 92
days_between_forecasts <- 7
forecast_horizon <- 30
starting_date <- as_date("2023-07-01") # from the start of a water year
all_forecast_dates <- seq.Date(starting_date, by = days_between_forecasts, length.out = num_forecasts)

# evaluation overflow -----------------------------------------------------
DA_period <- arrow::open_dataset('forecasts/parquet/') |>
  dplyr::filter(model_id == 'glm_flare_v3_crest',
                site_id == "ALEX",
                reference_date %in% all_forecast_dates,
                variable == 'overflow_flow') |> 
  collect() |> 
  filter(as_date(datetime) < as_date(reference_datetime)) |> 
  mutate(prediction = prediction/1000) # convert to ML/day

obs_outflow <- read_csv('targets/ALEX/ALEX-targets-outflow.csv', show_col_types = F) |> 
  filter(variable == 'FLOW') |> 
  mutate(variable = 'overflow_flow',
         observation = observation*86.4) # convert to ML/day

overflow_scores <- DA_period %>%
  dplyr::mutate(family = as.character(family)) |>
  score4cast::crps_logs_score(obs_outflow) |>
  dplyr::mutate(horizon = datetime-lubridate::as_datetime(reference_datetime)) |>
  dplyr::mutate(horizon = as.numeric(lubridate::as.duration(horizon),
                                     units = "seconds"),
                horizon = horizon / 86400,
                reference_date = lubridate::as_date(reference_datetime))

# plot comparison
overflow_scores |> 
  ggplot(aes(x=datetime, y=observation)) +
  geom_point() +
  geom_line(aes(y = median)) +
  geom_ribbon(aes(ymax = quantile97.5, ymin = quantile02.5), alpha = 0.3)


# evaluate in-situ forecasts -------------------------------------------------
eval_vars <- c('depth', 'salt', 'temperature')

forecast_s3 <- arrow::s3_bucket(bucket = config$s3$forecasts_parquet$bucket, endpoint_override = config$s3$forecasts_parquet$endpoint, anonymous = TRUE)
forecasts_df <- arrow::open_dataset('forecasts/parquet/') |>
  dplyr::filter(model_id == 'glm_flare_v3_crest',
                site_id == "ALEX",
                reference_date %in% all_forecast_dates,
                variable %in% eval_vars) |>
  dplyr::collect() |> 
  mutate(variable_type = 'state')


targets_df <- read_csv("targets/ALEX/ALEX-targets-insitu.csv",show_col_types = FALSE) 

scoring <- generate_forecast_score_arrow(targets_df = targets_df,
                                         forecast_df = forecasts_df, ## only works if dataframe returned from output
                                         use_s3 = FALSE,
                                         bucket = NULL,
                                         endpoint = NULL,
                                         local_directory = 'scores',
                                         variable_types = "state")

arrow::open_dataset('scores/site_id=ALEX/') |> 
  filter(model_id %in% c('climatology', 'glm_flare_v3_crest'),
         reference_date %in% all_forecast_dates,
         variable %in% eval_vars) |> 
  collect() |> 
  mutate(horizon = as.numeric(as_date(datetime) - as_date(reference_datetime))) |> 
  reframe(.by = all_of(c('horizon', 'variable', 'depth', 'model_id')),
          median_crps = median(crps, na.rm = T)) |> 
  filter(#variable == 'depth',
         depth == 0.5 | is.na(depth),
         horizon > 0) |> 
  ggplot(aes(x=horizon, y=median_crps, colour = model_id)) +
  geom_line() +
  facet_wrap(~variable, scales = 'free')
