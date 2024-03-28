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


download.file(url = "https://water.data.sa.gov.au/Export/DataSet?DataSet=Water%20Temp.Best%20Available--Continuous%40A4261133&DateRange=Days30&ExportFormat=csv&Compressed=false&RoundData=False&Unit=degC&Timezone=9.5&_=1668874574781",
              destfile = file.path(lake_directory, "data_raw", "current_water_temp.csv"))

cleaned_insitu_file <- file.path(config$file_path$qaqc_data_directory, paste0(config$location$site_id, "-targets-insitu.csv"))
#readr::read_csv(file.path(lake_directory, "data_raw", "current_water_temp.csv"), skip = 1, show_col_types = FALSE) |>
readr::read_csv(file.path(lake_directory, "data_raw", "current_water_temp.csv"), skip = 5, show_col_types = FALSE, col_names = c('Timestamp (UTC+09:30)', 'Value (°C)')) |> 
  rename(time = `Timestamp (UTC+09:30)`,
         observed = `Value (°C)`) |> 
  select(time, observed) |> 
  mutate(time = lubridate::force_tz(time, tzone = "Etc/GMT+9"),
         time = time - lubridate::minutes(30),
         time = lubridate::with_tz(time, tzone = "UTC"),
         date = lubridate::as_date(time),
         hour = lubridate::hour(time)) |>
  group_by(date, hour) |> 
  summarize(observation = mean(observed, na.rm = TRUE), .groups = "drop") |> 
  mutate(variable = "temperature",
         depth = 0.5,
         site_id = config$location$site_id,
         datetime = lubridate::as_datetime(date) + lubridate::hours(hour)) |> 
  filter(hour == 0) |> 
  select(site_id, datetime, depth, variable, observation) |> 
  write_csv(cleaned_insitu_file)

#' Move targets to s3 bucket

message("Successfully generated targets")

FLAREr::put_targets(site_id =  config$location$site_id,
                    cleaned_insitu_file,
                    cleaned_met_file = NA,
                    cleaned_inflow_file = NA,
                    use_s3 = config$run_config$use_s3,
                    config = config)

if(config$run_config$use_s3){
  message("Successfully moved targets to s3 bucket")
}
noaa_ready <- TRUE

while(noaa_ready){
  
  config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)
  
  
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
  
  RCurl::url.exists("https://hc-ping.com/31c3e142-8f8c-42ae-9edc-d277adb94b31", timeout = 5)
  
  noaa_ready <- FLAREr::check_noaa_present_arrow(lake_directory,
                                                 configure_run_file,
                                                 config_set_name = config_set_name)
  
  
}
