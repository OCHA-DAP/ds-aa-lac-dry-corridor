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
library(ecmwfr)
library(googledrive)
library(gghdx)
library(blastula)
library(sf)
library(terra)
library(rnaturalearth)

# 
# # library(sf)
# # library(googlesheets4)
# library(janitor)
# # library(tmap)
# library(here)
# library(ggtext) # colored title
# library(glue)
# library(rhdx)
gghdx()

ecmwf_leadimes <- c(1:6)
run_date <- Sys.Date()
# pub dates - 5th day of every month
#https://www.ecmwf.int/en/newsletter/154/meteorology/ecmwfs-new-long-range-forecasting-system-seas5

# source(file.path("R","email_funcs.R"))
# source(file.path("src","email","email_utils.R"))


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


#' Download ECMWF SEAS51 Data for the AOI (Nicaragua, Honduras, Guatemala, El Salvador)
#' Uses [ecmwfr package](https://github.com/bluegreen-labs/ecmwfr) package to access [ECMWF Climate Data Store API ](https://cds.climate.copernicus.eu/)
#' Registration instructions are available in teh above ecmwfr package link.




# think only need to do this 1x
ecmwfr::wf_set_key(
  user=Sys.getenv("ECMWF_USER_UID"),
  key = Sys.getenv("ECMWF_USER_KEY"),
  service = "cds"
)


# Check Metadata/attributes for appropriate selection ---------------------

## List data sets available
df_cds_datasets<- wf_datasets(
  user=Sys.getenv("ECMWF_USER_UID"),
  service = "cds"
)

# get metdata of that specific data set ("seasonal-monthly-single-levels")
prod_info <- wf_product_info(
  dataset = "seasonal-monthly-single-levels",
  user=Sys.getenv("ECMWF_USER_UID"),
  service = "cds"
)


cat("defining bbox for extraction\n")
aoi_countries <- ?ne_countries(country = c("Nicaragua","Honduras","Guatemala","El Salvador")) %>% 
  st_as_sf() %>% 
  select(
    contains("admin"),
    iso_a3
  )
aoi_bbox <- st_bbox(aoi_countries) 


# Create API requests ---------------------------------------------------------
pub_mo_date <- format(floor_date(run_date,"month"),"%Y%m%d")

cat("writing data requests to list\n")
request_coords<- glue("{aoi_bbox['ymin']}/{aoi_bbox['xmin']}/{aoi_bbox['ymax']}/{aoi_bbox['xmax']}")

ecmwf_data_request <- ecmwf_leadimes %>% 
  map(
    ~list(
      product_type = "monthly_mean",
      format = "grib",
      originating_centre = "ecmwf",
      system="51",
      variable = c("total_precipitation"),
      year = as.character(year(run_date)),
      month = sprintf("%02d",month(run_date)),
      area = request_coords,
      leadtime_month = .x,
      dataset_short_name = "seasonal-monthly-single-levels",
      target = glue("ecmwf_seas51_monthly_{pub_mo_date}_lt{.x}.grib")
    )
  )

