lake_directory <- here::here()

# Scores
download.file(url = "https://sandbox.zenodo.org/records/264303/files/scores.zip?download=1",
              destfile = file.path(lake_directory,"scores.zip"), method = "curl")
unzip(file.path(lake_directory,"scores.zip"))

# Drivers
download.file(url = "https://sandbox.zenodo.org/records/264303/files/drivers.zip?download=1",
              destfile = file.path(lake_directory,"drivers.zip"), method = "curl")
unzip(file.path(lake_directory,"drivers.zip"))

# Forecasts
download.file(url = "https://sandbox.zenodo.org/records/264303/files/forecasts.zip?download=1",
              destfile = file.path(lake_directory,"forecasts.zip"), method = "curl")
unzip(file.path(lake_directory,"forecasts.zip"))

# Targets (observations)
download.file(url = "https://sandbox.zenodo.org/records/264303/files/targets.zip?download=1",
              destfile = file.path(lake_directory,"targets.zip"), method = "curl")
unzip(file.path(lake_directory,"targets.zip"))



