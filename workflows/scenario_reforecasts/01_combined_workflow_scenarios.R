#--------------------------------------#
## Project: ALEX-reforecasts
## Script purpose: run weekly 30 day ahead-forecasts of ALEX for reforecast analysis
## Date: 2025-04-08
## Author: Freya Olsson
#--------------------------------------#
set.seed(100)
readRenviron("~/.Renviron") # MUST come first
library(tidyverse)
library(lubridate)
lake_directory <- here::here()
setwd(lake_directory)
forecast_site <- "ALEX"
configure_run_file <- "configure_run.yml"
config_set_name <- "glm_flare_v3"


Sys.setenv("AWS_DEFAULT_REGION" = "amnh1",
           "AWS_S3_ENDPOINT" = "osn.mghpcc.org",
           "USE_HTTPS" = TRUE,
           'GLM_PATH' = 'GLM3r')


starting_index <- 1

# check the run_config is set up correctly for these reforecasts
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


# Reforecasts to run -----------------------------
num_forecasts <- 92
days_between_forecasts <- 7
forecast_horizon <- 30
starting_date <- as_date("2023-07-01") # from the start of a water year
forecast_dates <- paste(seq.Date(starting_date, by = 7, length.out = num_forecasts), "00:00:00")
# ------------------------------------#


# Run forecasts!
for(i in starting_index:length(forecast_dates)) {
  
  message(paste0("index: ", i))
  message(paste0("     Running forecast: ", forecast_dates[i]))
  

  if (i == 1) {
    config <- FLAREr::set_up_simulation(configure_run_file,lake_directory, config_set_name = config_set_name,
                                        clean_start = T) # start a clean run with spin up
  } else {
    config <- FLAREr::set_up_simulation(configure_run_file,lake_directory, config_set_name = config_set_name,
                                        clean_start = F) # use restart
  }
  
  
  # Generate inflow/outflows
  source(file.path('workflows/scenario_reforecasts/baseline_inflow_workflow.R')) 
  # combined flow drivers - assuming inflows lagged from upstream and persistence outflow
  
  # run FLARE for DA period ----------------------
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
  
  DA_period <- arrow::read_parquet(file.path(config$file_path$forecast_output_directory,
                                             "parquet",
                                             paste0('site_id=', config$location$site_id),
                                             paste0('model_id=', config$run_config$sim_name),
                                             paste0('reference_date=', gsub(" 00:00:00", "", 
                                                                            config$run_config$forecast_start_datetime)),
                                             "part-0.parquet"))
  #------------------------------------------------------------#
  # Run FLARE during the forecasts ----------------------------
  # 1. copy restart file
  restart_file_name <- basename(output$restart_file)
  restart_forecast_file_name <- gsub(".nc", '_forecast.nc', restart_file_name)
  restart_directory_main <- config$file_path$restart_directory
  
  file.copy(file.path(restart_directory_main, restart_file_name),
            file.path(restart_directory_main, restart_forecast_file_name),
            overwrite = T)
  
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
  
  forecast_period <- arrow::read_parquet(file.path(config$file_path$forecast_output_directory,
                                                   "parquet",
                                                   paste0('site_id=', config$location$site_id),
                                                   paste0('model_id=', config$run_config$sim_name),
                                                   paste0('reference_date=', gsub(" 00:00:00", "", 
                                                                                  config$run_config$forecast_start_datetime)),
                                                   "part-0.parquet"))
  
  # combine with DA period and resave
  bind_rows(DA_period, forecast_period) |> 
    arrow::write_parquet(file.path(config$file_path$forecast_output_directory,
                                   "parquet",
                                   paste0('site_id=', config$location$site_id),
                                   paste0('model_id=', config$run_config$sim_name),
                                   paste0('reference_date=', gsub(" 00:00:00", "", 
                                                                  config$run_config$forecast_start_datetime)),
                                   "part-0.parquet"))
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
    
    file.copy(file.path(restart_directory_main, restart_forecast_file_name),
              file.path(restart_directory_scenario, restart_file_scenario),
              overwrite = T)
    
    
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
    
    # yaml::write_yaml(restart_run_config_yaml, 
    #                  file = file.path(config$file_path$configuration_directory, configure_run_file))
    # 
    FLAREr::update_run_config(lake_directory = lake_directory,
                              configure_run_file = configure_run_file,
                              restart_file = restart_file_scenario, # uses restart file from previous forecast
                              start_datetime = forecast_dates[i],
                              end_datetime = NA,
                              forecast_start_datetime = forecast_dates[i],
                              forecast_horizon = horizon, # no forecast, DA only
                              sim_name = scenario_sim_names$sim_name[j],
                              site_id = config$location$site_id,
                              configure_flare = config$run_config$configure_flare,
                              configure_obs = config$run_config$configure_obs,
                              use_s3 = config$run_config$use_s3,
                              bucket = config$s3$restart$bucket,
                              endpoint = config$s3$restart$endpoint,
                              use_https = TRUE)
    
    
    config <- FLAREr::set_up_simulation(configure_run_file, lake_directory, config_set_name = config_set_name,
                                        sim_name = scenario_sim_names$sim_name[j])
    
    # Run FLARE for scenario forecast period
    output <- FLAREr::run_flare(lake_directory = lake_directory,
                                configure_run_file = configure_run_file,
                                config_set_name = config_set_name,
                                sim_name = scenario_sim_names$sim_name[j])

    
    forecast_period_scenario <- arrow::read_parquet(file.path(config$file_path$forecast_output_directory,
                                                              "parquet",
                                                              paste0('site_id=', config$location$site_id),
                                                              paste0('model_id=', config$run_config$sim_name),
                                                              paste0('reference_date=', gsub(" 00:00:00", "", 
                                                                                             config$run_config$forecast_start_datetime)),
                                                              "part-0.parquet"))
    
    bind_rows(DA_period, forecast_period_scenario) |> 
      arrow::write_parquet(file.path(config$file_path$forecast_output_directory,
                                     "parquet",
                                     paste0('site_id=', config$location$site_id),
                                     paste0('model_id=', config$run_config$sim_name),
                                     paste0('reference_date=', gsub(" 00:00:00", "", 
                                                                    config$run_config$forecast_start_datetime)),
                                     "part-0.parquet"))
    
    
  }
  
  #-------------------------------------#
  # set up actual restart file - for next forecast date

  new_forecast_date <- lubridate::as_datetime(forecast_dates[i]) + lubridate::days(7) ## WEEKLY FORECAST
  new_start_datetime <- lubridate::as_datetime(new_forecast_date) - lubridate::days(7) ## SET LONGER LOOK BACK FOR DATA
  
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
  
  #RCurl::url.exists("https://hc-ping.com/31c3e142-8f8c-42ae-9edc-d277adb94b31", timeout = 5)
  
  # noaa_ready <- FLAREr::check_noaa_present(lake_directory,
  #                                          configure_run_file,
  #                                          config_set_name = config_set_name)
  
  rm(forecast_period, forecast_period_scenario, DA_period)
  gc()
}
