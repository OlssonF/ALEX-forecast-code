library(arrow)
library(tidyverse)

site_id_list <- c("ALEX")
model_id_list <- c('glm_flare_v3_crest', 'glm_flare_v3_crest_down_0.1', 'glm_flare_v3_crest_up_0.1',
                   'climatology', 'persistenceRW')

use_s3 <- F # persistence and climatology are not on s3!
num_forecasts <- 92
days_between_forecasts <- 7
starting_date <- as_date("2023-07-01") # from the start of a water year
all_forecast_dates <- as.character(seq.Date(starting_date, by = days_between_forecasts, length.out = num_forecasts))

lake_directory <- here::here()

# Archiving NOAA ----------
message("Archiving stage 2 NOAA")

s3_stage2 <- s3_bucket("bio230121-bucket01/flare/drivers/met/gefs-v12/stage2",
                       endpoint_override = "amnh1.osn.mghpcc.org", anonymous = TRUE)

df <- open_dataset(s3_stage2, partitioning = c("reference_datetime","site_id")) |>
  filter(site_id %in% site_id_list,
         reference_datetime %in% all_forecast_dates)

write_dataset(df,
              path = file.path(lake_directory, "archive/drivers/drivers/met/gefs-v12/stage2/"),
              hive_style = FALSE,
              partitioning = c("reference_datetime","site_id"))

message("Archiving stage 3 NOAA")

s3_stage3 <- s3_bucket("bio230121-bucket01/flare/drivers/met/gefs-v12/stage3", 
                       endpoint_override = "amnh1.osn.mghpcc.org", anonymous = TRUE)

df <- open_dataset(s3_stage3, partitioning = c("site_id")) |>
  filter(site_id %in% site_id_list)

write_dataset(df,
              path = file.path(lake_directory, "archive/drivers/drivers/met/gefs-v12/stage3"),
              hive_style = FALSE,
              partitioning = c("site_id"))

setwd(file.path(lake_directory, "archive/drivers"))
files2zip <- fs::dir_ls(recurse = TRUE)
files2zip <- files2zip[stringr::str_detect(files2zip, pattern = "DS_Store", negate = TRUE)][-1]
utils::zip(zipfile = file.path(lake_directory, "archive/drivers"), files = files2zip)

##############

# Archive forecasts -----------------------
message("Archiving forecast parquets")

if(use_s3){
  s3_forecast <- arrow::s3_bucket(bucket = "bio230121-bucket01/flare/forecasts/parquet/",
                                  endpoint_override =  "amnh1.osn.mghpcc.org", anonymous = TRUE)
}else{
  s3_forecast <- file.path(lake_directory, "forecasts/parquet/")
}

df_all <- open_dataset(s3_forecast) |>
  filter(site_id %in% site_id_list,
         model_id %in% model_id_list)

write_dataset(df_all, path = file.path(lake_directory, "archive/forecasts/forecasts/parquet"),
              hive_style = TRUE, partitioning = c("site_id","model_id","reference_date"))

setwd(file.path(lake_directory, "archive/forecasts"))
files2zip <- fs::dir_ls(recurse = TRUE)
files2zip <- files2zip[stringr::str_detect(files2zip, pattern = "DS_Store", negate = TRUE)][-1]
utils::zip(zipfile = file.path(lake_directory, "archive/forecasts"), files = files2zip)

#######

# Archiving scores ---------------------------
message("Archiving score parquets")

if(use_s3){
  s3_scores <- arrow::s3_bucket(bucket = "bio230121-bucket01/flare/scores/parquet",
                                endpoint_override =  "amnh1.osn.mghpcc.org",
                                anonymous = TRUE)
}else{
  s3_scores <- file.path(lake_directory, "scores/")
}

df_all <- open_dataset(s3_scores) |>
  filter(site_id %in% site_id_list,
         model_id %in% model_id_list)

write_dataset(df_all, path = file.path(lake_directory, "archive/scores/scores/"),
              hive_style = TRUE, partitioning = c("site_id","model_id", "reference_datetime"))


setwd(file.path(lake_directory, "archive/scores"))
files2zip <- fs::dir_ls(recurse = TRUE)
files2zip <- files2zip[stringr::str_detect(files2zip, pattern = "DS_Store", negate = TRUE)][-1]
utils::zip(zipfile = file.path(lake_directory, "archive/scores"), files = files2zip)

#######

# Archive targets ------------------------------
message("Archiving targets")

s3_targets <- file.path(lake_directory, "targets/")

fs::dir_create(file.path(lake_directory, "archive/targets"))

file.copy(from = s3_targets, paste0(lake_directory,"/archive/targets"),
          overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)

setwd(file.path(lake_directory, "archive/targets"))
files2zip <- fs::dir_ls(recurse = TRUE)
files2zip <- files2zip[stringr::str_detect(files2zip, pattern = "DS_Store", negate = TRUE)][-1]
utils::zip(zipfile = file.path(lake_directory, "archive/targets"), files = files2zip)
