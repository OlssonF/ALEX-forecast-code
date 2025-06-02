# install packages
install.packages(c("remotes", "tidyverse", "lubridate", "arrow", "fable", "fabletools", "imputeTS", "tidymodels", "tsibble", "ggpubr", "ggrepel", "gsw"))
install.packages(c("here", "aws.s3"))

remotes::install_github("FLARE-forecast/FLAREr", ref = "v3.0.3")
