readRenviron("~/.Renviron") # MUST come first
library(tidyverse)
library(lubridate)
lake_directory <- here::here()
setwd(lake_directory)
forecast_site <- "ALEX"
configure_run_file <- "configure_run.yml"
config_set_name <- "default"

fresh_run <- TRUE

Sys.setenv("AWS_DEFAULT_REGION" = "renc",
           "AWS_S3_ENDPOINT" = "osn.xsede.org",
           "USE_HTTPS" = TRUE)

message("Checking for NOAA forecasts")

config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)

if(fresh_run) unlink(file.path(lake_directory, "restart", "ALEX", config$run_config$sim_name, configure_run_file))

# # In-situ lake level, water temp, salinity (EC)
# water level data is height above sea level
# height of bottom of ALEX (per glm.nml) = -5.3 m
# In situ WQ data
options(timeout=300)
download.file(url = paste0("https://water.data.sa.gov.au/Export/BulkExport?DateRange=Custom&StartTime=2020-01-01%2000%3A00&EndTime=", Sys.Date(), "%2000%3A00&TimeZone=0&Calendar=CALENDARYEAR&Interval=PointsAsRecorded&Step=1&ExportFormat=csv&TimeAligned=True&RoundData=True&IncludeGradeCodes=False&IncludeApprovalLevels=False&IncludeQualifiers=False&IncludeInterpolationTypes=False&Datasets[0].DatasetName=Lake%20Level.Best%20Available--Continuous%40A4261133&Datasets[0].Calculation=Instantaneous&Datasets[0].UnitId=82&Datasets[1].DatasetName=EC%20Corr.Best%20Available%40A4261133&Datasets[1].Calculation=Instantaneous&Datasets[1].UnitId=305&Datasets[2].DatasetName=Water%20Temp.Best%20Available--Continuous%40A4261133&Datasets[2].Calculation=Instantaneous&Datasets[2].UnitId=169&_=1711554907800"),
              destfile = file.path(lake_directory, "data_raw", "current_insitu.csv"))

cleaned_insitu_file <- file.path(config$file_path$qaqc_data_directory, paste0(config$location$site_id, "-targets-insitu.csv"))

readr::read_csv(file.path(lake_directory, "data_raw", "current_insitu.csv"), 
                skip = 5, show_col_types = FALSE, 
                col_names = c('time','Value_level', 'Value_EC', 'Value_temperature')) |> 
  # simple conversion to salt
  mutate(Value_salt = oce::swSCTp(conductivity = Value_EC/1000,
                                  temperature = Value_temperature, 
                                  conductivityUnit = 'mS/cm'),
         Value_depth = 5.3 + Value_level) |> # 5.3 is the height 
  select(-Value_EC, -Value_level) |> 
  pivot_longer(names_to = 'variable', names_prefix = 'Value_',
               cols = starts_with('Value'), 
               values_to = 'observed') |> 
  mutate(time = lubridate::force_tz(time, tzone = "Etc/GMT+9"),
         time = time - lubridate::minutes(30),
         time = lubridate::with_tz(time, tzone = "UTC"),
         date = lubridate::as_date(time),
         hour = lubridate::hour(time)) |>
  group_by(date, hour, variable) |> 
  summarize(observation = mean(observed, na.rm = TRUE), .groups = "drop") |> 
  mutate(depth = ifelse(variable %in% c('salt', 'temperature'), 0.5, NA),
         site_id = config$location$site_id,
         datetime = lubridate::as_datetime(date) + lubridate::hours(hour)) |> 
  filter(hour == 0) |> 
  select(site_id, datetime, depth, variable, observation) |> 
  write_csv(cleaned_insitu_file)


# Inflow targets
# Currently getting discharge, temp, and conductivity - data is in UTC
options(timeout=300)
download.file(paste0('https://water.data.sa.gov.au/Export/BulkExport?DateRange=Custom&StartTime=2020-01-01%2000%3A00&EndTime=', Sys.Date(), '%2000%3A00&TimeZone=0&Calendar=CALENDARYEAR&Interval=PointsAsRecorded&Step=1&ExportFormat=csv&TimeAligned=True&RoundData=True&IncludeGradeCodes=False&IncludeApprovalLevels=False&IncludeQualifiers=False&IncludeInterpolationTypes=False&Datasets[0].DatasetName=Discharge.Master--Daily%20Read--ML%2Fday%40A4260903&Datasets[0].Calculation=Instantaneous&Datasets[0].UnitId=239&Datasets[1].DatasetName=EC%20Corr.Best%20Available--Sensor%20near%20surface%40A4261159&Datasets[1].Calculation=Instantaneous&Datasets[1].UnitId=305&Datasets[2].DatasetName=Water%20Temp.Best%20Available--Sensor%20near%20surface%40A4261159&Datasets[2].Calculation=Instantaneous&Datasets[2].UnitId=169&_=1711479354464'),
              destfile = file.path(lake_directory, "data_raw", "current_inflow.csv"))

cleaned_inflow_file <- file.path(config$file_path$qaqc_data_directory, paste0(config$location$site_id, "-targets-inflow.csv"))

readr::read_csv(file.path(lake_directory, "data_raw", "current_inflow.csv"), 
                skip = 5, show_col_types = FALSE, 
                col_names = c('time','Value_FLOW', 'Value_EC', 'Value_TEMP')) |> 
  # simple conversion to salt
  mutate(Value_SALT = oce::swSCTp(conductivity = Value_EC/1000,
                                  temperature = Value_TEMP, 
                                  conductivityUnit = 'mS/cm')) |> 
  select(-Value_EC) |> 
  pivot_longer(names_to = 'variable',
               names_prefix = 'Value_',
               cols = -time, 
               values_to = 'observed') |> 
  mutate(time = lubridate::with_tz(time, tzone = "UTC"),
         date = lubridate::as_date(time),
         hour = lubridate::hour(time)) |>
  group_by(date, variable) |> # calculate the daily mean - assign this to midnight
  summarize(observation = mean(observed, na.rm = TRUE), .groups = "drop") |> 
  mutate(site_id = config$location$site_id,
         datetime = lubridate::as_datetime(paste(date, '00:00:00'))) |> # assigned to midnight
  select(site_id, datetime, variable, observation) |> 
  write_csv(cleaned_inflow_file)
#' Move targets to s3 bucket

message("Successfully generated targets")

FLAREr::put_targets(site_id =  config$location$site_id,
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
  
  
  config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)
  
  
  # Generate inflow forecast
  source(file.path('workflows', config_set_name,'run_inflow_DOYforecast.R'))
  
  # run FLARE forecast
  output <- FLAREr::run_flare(lake_directory = lake_directory,
                              configure_run_file = configure_run_file,
                              config_set_name = config_set_name)
  
  forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime) + lubridate::days(1)
  start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime) - lubridate::days(5) ## SET LONGER LOOK BACK FOR DATA
  restart_file <- paste0(config$location$site_id,"-", (lubridate::as_date(forecast_start_datetime)- days(1)), "-",config$run_config$sim_name ,".nc")
  
  FLAREr::update_run_config2(lake_directory = lake_directory,
                             configure_run_file = configure_run_file, 
                             restart_file = basename(output$restart_file), 
                             start_datetime = lubridate::as_datetime(config$run_config$start_datetime) + lubridate::days(1), 
                             end_datetime = NA, 
                             forecast_start_datetime = lubridate::as_datetime(config$run_config$forecast_start_datetime) + lubridate::days(1),  
                             forecast_horizon = config$run_config$forecast_horizon,
                             sim_name = config$run_config$sim_name, 
                             site_id = config$location$site_id,
                             configure_flare = config$run_config$configure_flare, 
                             configure_obs = config$run_config$configure_obs, 
                             use_s3 = config$run_config$use_s3,
                             bucket = config$s3$warm_start$bucket,
                             endpoint = config$s3$warm_start$endpoint,
                             use_https = TRUE)
  
  #RCurl::url.exists("https://hc-ping.com/31c3e142-8f8c-42ae-9edc-d277adb94b31", timeout = 5)
  
  noaa_ready <- FLAREr::check_noaa_present_arrow(lake_directory,
                                                 configure_run_file,
                                                 config_set_name = config_set_name)
  
  
}
