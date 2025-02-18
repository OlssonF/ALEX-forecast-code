set.seed(100)
readRenviron("~/.Renviron") # MUST come first
library(tidyverse)
library(lubridate)
lake_directory <- here::here()
setwd(lake_directory)
forecast_site <- "ALEX"
configure_run_file <- "configure_run.yml"
config_set_name <- "glm_flare_v3"

fresh_run <- TRUE

Sys.setenv("AWS_DEFAULT_REGION" = "renc",
           "AWS_S3_ENDPOINT" = "osn.xsede.org",
           "USE_HTTPS" = TRUE,
           'GLM_PATH' = 'GLM3r')

source('R/generate_forecast_score_arrow.R')

message("Checking for NOAA forecasts")

config <- FLAREr::set_up_simulation(configure_run_file,lake_directory, config_set_name = config_set_name,
                                    clean_start = fresh_run)

# if(fresh_run) unlink(file.path(lake_directory, "restart", "ALEX", config$run_config$sim_name, configure_run_file))

# Generate targets
source(file.path('workflows', config_set_name, 'generate_targets.R'))

message("Successfully generated targets")

FLAREr:::put_targets(site_id =  config$location$site_id,
                     cleaned_insitu_file,
                     cleaned_met_file = NA,
                     cleaned_inflow_file,
                     use_s3 = config$run_config$use_s3,
                     config = config)

if(config$run_config$use_s3){
  message("Successfully moved targets to s3 bucket")
}
noaa_ready <- TRUE



while(noaa_ready){
  
  config <- FLAREr::set_up_simulation(configure_run_file,lake_directory, config_set_name = config_set_name)
  
  # Generate inflow/outflows
  source(file.path('workflows', config_set_name,'new_baseline_inflow_workflow.R')) 
  # combined flow drivers - assuming inflows lagged from upstream and persistence outflow

  # run FLARE forecast
  output <- FLAREr::run_flare(lake_directory = lake_directory,
                              configure_run_file = configure_run_file,
                              config_set_name = config_set_name)
  
  message("Scoring forecasts")
  forecast_s3 <- arrow::s3_bucket(bucket = config$s3$forecasts_parquet$bucket, endpoint_override = config$s3$forecasts_parquet$endpoint, anonymous = TRUE)
  forecast_df <- arrow::open_dataset(forecast_s3) |>
    dplyr::mutate(reference_date = lubridate::as_date(reference_date)) |>
    dplyr::filter(model_id == 'glm_flare_v3',
                  site_id == forecast_site,
                  reference_date == lubridate::as_datetime(config$run_config$forecast_start_datetime)) |>
    dplyr::collect()
  
  if(config$output_settings$evaluate_past & config$run_config$use_s3){
    #past_days <- lubridate::as_date(forecast_df$reference_datetime[1]) - lubridate::days(config$run_config$forecast_horizon)
    past_days <- lubridate::as_date(lubridate::as_date(config$run_config$forecast_start_datetime) - lubridate::days(config$run_config$forecast_horizon))
    
    #vars <- arrow_env_vars()
    past_s3 <- arrow::s3_bucket(bucket = config$s3$forecasts_parquet$bucket, endpoint_override = config$s3$forecasts_parquet$endpoint, anonymous = TRUE)
    past_forecasts <- arrow::open_dataset(past_s3) |>
      dplyr::mutate(reference_date = lubridate::as_date(reference_date)) |>
      dplyr::filter(model_id == 'glm_flare_v3',
                    site_id == forecast_site,
                    reference_date == past_days) |>
      dplyr::collect()
    #unset_arrow_vars(vars)
  }else{
    past_forecasts <- NULL
  }
  
  combined_forecasts <- dplyr::bind_rows(forecast_df, past_forecasts)
  
  targets_df <- read_csv(file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),show_col_types = FALSE)
  
  #combined_forecasts <- arrow::open_dataset('./forecasts/parquet/site_id=ccre/model_id=glm_flare_v3/reference_date=2024-09-03/part-0.parquet') |> collect()
  
  
  
  scoring <- generate_forecast_score_arrow(targets_df = targets_df,
                                           forecast_df = combined_forecasts, ## only works if dataframe returned from output
                                           use_s3 = TRUE,
                                           bucket = config$s3$scores$bucket,
                                           endpoint = config$s3$scores$endpoint,
                                           local_directory = './ALEX-forecast-code/scores/ALEX',
                                           variable_types = c("state","parameter"))
   
  forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime) + lubridate::days(1)
  start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime) - lubridate::days(5) ## SET LONGER LOOK BACK FOR DATA
  restart_file <- paste0(config$location$site_id,"-", (lubridate::as_date(forecast_start_datetime)- days(1)), "-",config$run_config$sim_name ,".nc")
  
  FLAREr::update_run_config(lake_directory = lake_directory,
                            configure_run_file = configure_run_file,
                            restart_file = basename(output$restart_file),
                            start_datetime = start_datetime,
                            end_datetime = NA,
                            forecast_start_datetime = forecast_start_datetime,
                            forecast_horizon = config$run_config$forecast_horizon,
                            sim_name = config$run_config$sim_name,
                            site_id = config$location$site_id,
                            configure_flare = config$run_config$configure_flare,
                            configure_obs = config$run_config$configure_obs,
                            use_s3 = config$run_config$use_s3,
                            bucket = config$s3$restart$bucket,
                            endpoint = config$s3$restart$endpoint,
                            use_https = TRUE)
  
  #RCurl::url.exists("https://hc-ping.com/31c3e142-8f8c-42ae-9edc-d277adb94b31", timeout = 5)
  
  noaa_ready <- FLAREr::check_noaa_present(lake_directory,
                                           configure_run_file,
                                           config_set_name = config_set_name)
  
  
}
