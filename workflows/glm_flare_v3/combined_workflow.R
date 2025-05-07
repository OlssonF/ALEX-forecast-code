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

Sys.setenv("AWS_DEFAULT_REGION" = "amnh1",
           "AWS_S3_ENDPOINT" = "osn.mghpcc.org",
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
                     cleaned_met_file,
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
  source(file.path('workflows', config_set_name,'baseline_inflow_workflow.R')) 
  # combined flow drivers - assuming inflows lagged from upstream and persistence outflow

  # run FLARE forecast
  # run for DA period --------------------------
  horizon <- config$run_config$forecast_horizon # extract the actual info from the config
  start_date <- config$run_config$start_datetime
  forecast_date <- config$run_config$forecast_start_datetime
  
  FLAREr::update_run_config(lake_directory = lake_directory,
                            configure_run_file = configure_run_file,
                            restart_file = config$run_config$restart_file, # uses restart file from previous forecast
                            start_datetime = config$run_config$start_datetime,
                            end_datetime = NA,
                            forecast_start_datetime = config$run_config$forecast_start_datetime,
                            forecast_horizon = 0, # no forecast, DA only
                            sim_name = config$run_config$sim_name,
                            site_id = config$location$site_id,
                            configure_flare = config$run_config$configure_flare,
                            configure_obs = config$run_config$configure_obs,
                            use_s3 = config$run_config$use_s3,
                            bucket = config$s3$restart$bucket,
                            endpoint = config$s3$restart$endpoint,
                            use_https = TRUE)
  
  # make sure config_flare has the right DA settings
  config_flare <- file.path(lake_directory, 'configuration', config_set_name, 'configure_flare.yml')
  config_flare_yaml <- yaml::read_yaml(config_flare) 
  config_flare_yaml$da_setup$par_fit_method <- "perturb"
  yaml::write_yaml(config_flare_yaml, config_flare) 
  
  
  config <- FLAREr::set_up_simulation(configure_run_file,lake_directory, config_set_name = config_set_name)
  
  output <- FLAREr::run_flare(lake_directory = lake_directory,
                              configure_run_file = configure_run_file,
                              config_set_name = config_set_name)
  
  if (use_s3 == T) {
    DA_period <- arrow::s3_bucket(bucket = file.path(config$s3$forecasts_parquet$bucket,
                                                     paste0('site_id=', config$location$site_id),
                                                     paste0('model_id=', config$run_config$sim_name),
                                                     paste0('reference_date=', gsub(" 00:00:00", "", 
                                                                                    config$run_config$forecast_start_datetime)),
                                                     "part-0.parquet"), 
                                     endpoint_override = config$s3$forecasts_parquet$endpoint) |> 
      arrow::read_parquet()
  } else {
    DA_period <- arrow::read_parquet(file.path(config$file_path$forecast_output_directory,
                                               "parquet",
                                               paste0('site_id=', config$location$site_id),
                                               paste0('model_id=', config$run_config$sim_name),
                                               paste0('reference_date=', gsub(" 00:00:00", "", 
                                                                              config$run_config$forecast_start_datetime)),
                                               "part-0.parquet"))  
  }
  
  #------------------------------------------------------------#
  
  
  # Run FLARE during the forecasts ----------------------------
  # 1. copy restart file
  restart_file_name <- basename(output$restart_file)
  restart_forecast_file_name <- gsub(".nc", '_forecast.nc', restart_file_name)
  restart_directory_main <- config$file_path$restart_directory
  
  if (config$run_config$use_s3 == T) {
    file.copy(file.path(restart_directory_main, restart_file_name),
              file.path(restart_directory_main, restart_forecast_file_name),
              overwrite = T)
    FLAREr:::put_restart_file(saved_file = file.path(restart_directory_main, restart_forecast_file_name),
                              config = config)
  } else {
    file.copy(file.path(restart_directory_main, restart_file_name),
              file.path(restart_directory_main, restart_forecast_file_name),
              overwrite = T)
    
  }
  
  # 2. edit config_run from restart
  restart_run_config <- file.path(config$file_path$restart_directory, configure_run_file)
  
  restart_run_config_yaml <- yaml::read_yaml(restart_run_config)
  restart_run_config_yaml$restart_file <- restart_forecast_file_name
  restart_run_config_yaml$start_datetime <- forecast_dates[i]
  restart_run_config_yaml$forecast_start_datetime <- forecast_dates[i]
  restart_run_config_yaml$forecast_horizon <- horizon
  
  yaml::write_yaml(restart_run_config_yaml, file = restart_run_config)
  
  # 3. edit the config_flare to change parameter perturb settings 
  config_flare <- file.path(lake_directory, 'configuration', config_set_name, 'configure_flare.yml')
  config_flare_yaml <- yaml::read_yaml(config_flare) 
  config_flare_yaml$da_setup$par_fit_method <- "perturb_init"
  yaml::write_yaml(config_flare_yaml, config_flare) 
  
  # Run FLARE for forecast period
  output <- FLAREr::run_flare(lake_directory = lake_directory,
                              configure_run_file = configure_run_file,
                              config_set_name = config_set_name)
  
  
  if (use_s3 == T) {
    forecast_period <- arrow::s3_bucket(bucket = file.path(config$s3$forecasts_parquet$bucket,
                                                     paste0('site_id=', config$location$site_id),
                                                     paste0('model_id=', config$run_config$sim_name),
                                                     paste0('reference_date=', gsub(" 00:00:00", "", 
                                                                                    config$run_config$forecast_start_datetime)),
                                                     "part-0.parquet"), 
                                  endpoint_override = config$s3$forecasts_parquet$endpoint) |> 
      arrow::read_parquet()
  } else {
    
    forecast_period <- arrow::read_parquet(file.path(config$file_path$forecast_output_directory,
                                                     "parquet",
                                                     paste0('site_id=', config$location$site_id),
                                                     paste0('model_id=', config$run_config$sim_name),
                                                     paste0('reference_date=', gsub(" 00:00:00", "", 
                                                                                    config$run_config$forecast_start_datetime)),
                                                     "part-0.parquet"))
  }
  
  
  # combine with DA period and resave
  if (config$run_config$use_s3 == T) {
    output_directory <- arrow::s3_bucket(bucket = config$s3$forecasts_parquet$bucket,
                                         endpoint_override = config$s3$forecasts_parquet$endpoint)
    
    bind_rows(DA_period, forecast_period) |> 
      mutate(site_id = config$location$site_id,
             model_id = config$run_config$sim_name,
             reference_date = gsub(" 00:00:00", "", 
                                   config$run_config$forecast_start_datetime)) |> 
      arrow::write_dataset(path = output_directory,
                           partitioning = c("site_id", "model_id","reference_date"))
  } else {
    bind_rows(DA_period, forecast_period) |> 
      arrow::write_parquet(file.path(config$file_path$forecast_output_directory,
                                     "parquet",
                                     paste0('site_id=', config$location$site_id),
                                     paste0('model_id=', config$run_config$sim_name),
                                     paste0('reference_date=', gsub(" 00:00:00", "", 
                                                                    config$run_config$forecast_start_datetime)),
                                     "part-0.parquet"))
  }
  
  #------------------------------------------------------------------#
  
  
  # Scenarios -------------------------
  scenario_sim_names <- expand.grid(dir = c('up', 'down'), crest_elev_change = c(0.1)) |> 
    mutate(sim_name = paste0("glm_flare_v3_crest_", dir, "_", crest_elev_change))
  
  # runs each scenario
  for (j in 1:nrow(scenario_sim_names)) {
    
    message("*** Running scenario ", scenario_sim_names$sim_name[j])
    # edit the restart file to change parameter values
    restart_file_scenario <- gsub('forecast', paste(scenario_sim_names$dir[j], 
                                                    scenario_sim_names$crest_elev_change[j],
                                                    sep = "_"),
                                  restart_forecast_file_name)
    
    restart_directory_scenario <- file.path(lake_directory, "restart", 
                                            config$location$site_id, 
                                            scenario_sim_names$sim_name[j])
    
    dir.create(restart_directory_scenario, recursive = TRUE, showWarnings = FALSE)
    
    
    if (config$run_config$use_s3 == T) {
      file.copy(file.path(restart_directory_main, restart_file_name),
                file.path(restart_directory_scenario, restart_forecast_scenario),
                overwrite = T)
      FLAREr:::put_restart_file(saved_file = file.path(restart_directory_scenario,
                                                       restart_forecast_scenario),
                                config = config)
    } else {
      file.copy(file.path(restart_directory_main, restart_forecast_file_name),
                file.path(restart_directory_scenario, restart_file_scenario),
                overwrite = T)
      
    }
    
   
    
    nc_restart <- ncdf4::nc_open(file.path(restart_directory_scenario, restart_file_scenario), 
                                 write = T)
    
    if (scenario_sim_names$dir[j] == 'up') {
      crest_elev <- ncdf4::ncvar_get(nc_restart, 'crest_elev') + scenario_sim_names$crest_elev_change[j]
    } else {
      crest_elev <- ncdf4::ncvar_get(nc_restart, 'crest_elev') - scenario_sim_names$crest_elev_change[j]
    }
    
    ncdf4::ncvar_put(nc_restart, varid = 'crest_elev', vals = crest_elev)
    ncdf4::nc_close(nc_restart)
    
    # edit config_run from restart
    scenario_run_config <- gsub('crest', paste("crest", scenario_sim_names$dir[j], 
                                               scenario_sim_names$crest_elev_change[j],
                                               sep = "_"),
                                restart_run_config)
    
    restart_run_config_yaml$sim_name <- scenario_sim_names$sim_name[j]
    

    FLAREr::update_run_config(lake_directory = lake_directory,
                              configure_run_file = configure_run_file,
                              restart_file = restart_file_scenario, # uses restart file from previous forecast
                              start_datetime = forecast_dates[i],
                              end_datetime = NA,
                              forecast_start_datetime = forecast_dates[i],
                              forecast_horizon = horizon, 
                              sim_name = scenario_sim_names$sim_name[j],
                              site_id = config$location$site_id,
                              configure_flare = config$run_config$configure_flare,
                              configure_obs = config$run_config$configure_obs,
                              use_s3 = config$run_config$use_s3, 
                              bucket = config$s3$restart$bucket,
                              endpoint = config$s3$restart$endpoint,
                              use_https = TRUE)
    
    
    config <- FLAREr::set_up_simulation(configure_run_file, lake_directory, 
                                        config_set_name = config_set_name,
                                        sim_name = scenario_sim_names$sim_name[j])
    
    # Run FLARE for scenario forecast period
    output <- FLAREr::run_flare(lake_directory = lake_directory,
                                configure_run_file = configure_run_file,
                                config_set_name = config_set_name,
                                sim_name = scenario_sim_names$sim_name[j])
    
    
    if (use_s3 == T) {
      forecast_period_scenario <- arrow::s3_bucket(bucket = file.path(config$s3$forecasts_parquet$bucket,
                                                             paste0('site_id=', config$location$site_id),
                                                             paste0('model_id=', config$run_config$sim_name),
                                                             paste0('reference_date=', gsub(" 00:00:00", "", 
                                                                                            config$run_config$forecast_start_datetime)),
                                                             "part-0.parquet"), 
                                          endpoint_override = config$s3$forecasts_parquet$endpoint) |> 
        arrow::read_parquet()
    } else {
      
      forecast_period_scenario <- arrow::read_parquet(file.path(config$file_path$forecast_output_directory,
                                                                "parquet",
                                                                paste0('site_id=', config$location$site_id),
                                                                paste0('model_id=', config$run_config$sim_name),
                                                                paste0('reference_date=', gsub(" 00:00:00", "", 
                                                                                               config$run_config$forecast_start_datetime)),
                                                                "part-0.parquet"))
    }
    
    
    # combine with DA period and resave
    if (config$run_config$use_s3 == T) {
      output_directory <- arrow::s3_bucket(bucket = config$s3$forecasts_parquet$bucket,
                                           endpoint_override = config$s3$forecasts_parquet$endpoint)
      
      bind_rows(DA_period, forecast_period_scenario) |> 
        mutate(site_id = config$location$site_id,
               model_id = config$run_config$sim_name,
               reference_date = gsub(" 00:00:00", "", 
                                     config$run_config$forecast_start_datetime)) |> 
        arrow::write_dataset(path = output_directory,
                             partitioning = c("site_id", "model_id","reference_date"))
    } else {
      
      bind_rows(DA_period, forecast_period_scenario) |> 
        arrow::write_parquet(file.path(config$file_path$forecast_output_directory,
                                       "parquet",
                                       paste0('site_id=', config$location$site_id),
                                       paste0('model_id=', config$run_config$sim_name),
                                       paste0('reference_date=', gsub(" 00:00:00", "", 
                                                                      config$run_config$forecast_start_datetime)),
                                       "part-0.parquet"))
    }
    

  }
  
  #-------------------------------------#
  # set up actual restart file - for next forecast date
  
  new_forecast_date <- lubridate::as_datetime(forecast_date) + lubridate::days(1) ## daily forecast
  new_start_datetime <- lubridate::as_datetime(new_forecast_date) - lubridate::days(5) ## SET LONGER LOOK BACK FOR DATA
  
  FLAREr::update_run_config(lake_directory = lake_directory,
                            configure_run_file = configure_run_file,
                            restart_file = restart_forecast_file_name, # use the previous restart
                            start_datetime = new_start_datetime, # reset the config
                            end_datetime = NA,
                            forecast_start_datetime = new_forecast_date, # reset the config
                            forecast_horizon = horizon, # reset the config
                            sim_name = 'glm_flare_v3_crest',
                            site_id = config$location$site_id,
                            configure_flare = config$run_config$configure_flare,
                            configure_obs = config$run_config$configure_obs,
                            use_s3 = config$run_config$use_s3,
                            bucket = config$s3$restart$bucket,
                            endpoint = config$s3$restart$endpoint,
                            use_https = TRUE)
  
  #==============================================================================#
  
  message("Scoring forecasts")
  forecast_s3 <- arrow::s3_bucket(bucket = config$s3$forecasts_parquet$bucket, 
                                  endpoint_override = config$s3$forecasts_parquet$endpoint, anonymous = TRUE)
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
      dplyr::filter(model_id == config$run_config$sim_name,
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
   
  
  #RCurl::url.exists("https://hc-ping.com/31c3e142-8f8c-42ae-9edc-d277adb94b31", timeout = 5)
  
  noaa_ready <- FLAREr::check_noaa_present(lake_directory,
                                           configure_run_file,
                                           config_set_name = config_set_name)
  
  
}
