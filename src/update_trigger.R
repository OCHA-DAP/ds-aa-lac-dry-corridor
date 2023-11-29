#' update trigger


# https://code.earthengine.google.com/?project=ds-chd-cadc
# library(rgee)
# ee_Initialize(
#   cloud_api_key = Sys.getenv("GEE_CADC_API_KEY")
# )


# libs --------------------------------------------------------------------
# in dedicated monitoring repo should consider setting up w/ {renv}
# have to load tidyverse packages separate for GHA for some reason
library(dplyr)
library(purrr)
library(readr)
library(ggplot2)
library(tidyr) # pivot wider
library(stringr)
library(lubridate)
# library(sf)
# library(googlesheets4)
library(janitor)
# library(tmap)
library(here)
library(blastula)
library(googledrive)
library(ggtext) # colored title
library(glue)
library(gghdx)
library(rhdx)
gghdx()



source(file.path("R","email_funcs.R"))
source(file.path("src","email","email_utils.R"))




# Google Drive ------------------------------------------------------------

drive_auth(
  path = Sys.getenv("CADC_MONITORING_JSON")
)
  
drive_dribble <- drive_ls(
  corpus = "user"
)

cadc_gha_dribble <- drive_dribble %>% 
  filter(name == "CADC_GHA")


# Check drive -------------------------------------------------------------
drive_download(
  as_id(
    drive_dribble %>% 
      filter(
        str_detect(name, "_log\\.csv$") 
      )%>% 
      pull(id)
  ),
  path = fp_dl_log <- tempfile(fileext = ".csv")
)

df_dl_log <- read_csv(fp_dl_log)


## Load Thresholds #####
## currently place holder - for df that should have country and threshold for season.
drive_download(
  as_id(
    drive_dribble %>% 
      filter(
        str_detect(name, "_thresholds\\.csv$") 
      )%>% 
      pull(id)
  ),
  path = fp_thresholds_log <- tempfile(fileext = ".csv")
)

df_thresholds <- read_csv(dl_log_path)



# Latest Forecast ---------------------------------------------------------

# get latest forecast as grib.


# wrangle latest forecast to tif -- this is currently done in python ... therefore i wonder if this should just be a
# hybrid notebook or if we sho






