library(tidyverse)
library(tidymodels)
library(xgboost)

# source('R/fct_awss3Connect_sensorcode.R')
# source('R/fct_awss3Connect.R')

source('R/inflows/run_inflow_model.R')
source('R/inflows/run_inflow_flow_model.R')
source('R/inflows/run_inflow_temperature_model.R')
source('R/inflows/run_inflow_salinity_model.R')

lake_directory <- here::here()
config_set_name <- "default"
forecast_site <- 'ALEX'
configure_run_file <- "configure_run.yml"
config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)

config_obs <- FLAREr::initialize_obs_processing(lake_directory, 
                                                observation_yml = "observation_processing.yml", 
                                                config_set_name = config_set_name)

FLAREr::get_targets(lake_directory, config)


message("Forecasting inflow and outflows")

# what dates need forecasting
reference_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
noaa_date <- reference_datetime - lubridate::days(1)
endpoint <- 'renc.osn.xsede.org'

## Run Forecast
inflow_forecast <- run_inflow_model(site_id = forecast_site, 
                                    forecast_start_datetime = reference_datetime, 
                                    use_s3_inflow = config$run_config$use_s3, 
                                    inflow_bucket = config$s3$inflow_drivers$bucket,
                                    inflow_endpoint = config$s3$inflow_drivers$endpoint,
                                    inflow_local_directory = file.path(lake_directory, config$inflow$local_directory), 
                                    forecast_horizon = config$run_config$forecast_horizon, 
                                    inflow_model = config$inflow$forecast_inflow_model)
