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
library(ecmwfr)
library(glue)


gghdx()

ecmwf_leadimes <- c(1:6)
run_date <- Sys.Date()
# pub dates - 5th day of every month
# https://www.ecmwf.int/en/newsletter/154/meteorology/ecmwfs-new-long-range-forecasting-system-seas5

# source(file.path("R","email_funcs.R"))
# source(file.path("src","email","email_utils.R"))


# Google Drive ------------------------------------------------------------

# authorize drive access
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
      ) %>%
      pull(id)
  ),
  path = fp_dl_log <- tempfile(fileext = ".csv")
)

df_dl_log <- read_csv(fp_dl_log)


## Load Thresholds #####
## currently place holder - for df that should have country and threshold for season.
# drive_download(
#   as_id(
#     drive_dribble %>%
#       filter(
#         str_detect(name, "_thresholds\\.csv$")
#       )%>%
#       pull(id)
#   ),
#   path = fp_thresholds_log <- tempfile(fileext = ".csv")
# )
#
# df_thresholds <- read_csv(dl_log_path)



# Latest Forecast ---------------------------------------------------------

# get latest forecast as grib.


# wrangle latest forecast to tif -- this is currently done in python ... therefore i wonder if this should just be a
# hybrid notebook or if we sho


#' Download ECMWF SEAS51 Data for the AOI (Nicaragua, Honduras, Guatemala, El Salvador)
#' Uses [ecmwfr package](https://github.com/bluegreen-labs/ecmwfr) package to access [ECMWF Climate Data Store API ](https://cds.climate.copernicus.eu/)
#' Registration instructions are available in teh above ecmwfr package link.


cat("set keyring options for linux\n")
# set options
options(keyring_backend="file")

# spoof keyring
if(!("ecmwfr" %in% keyring::keyring_list()$keyring)){
  keyring::keyring_create("ecmwfr", password = "test")
}

cat("setting key\n")
ecmwfr::wf_set_key(
  user = Sys.getenv("ECMWF_USER_UID"),
  key = Sys.getenv("ECMWF_USER_KEY"),
  service = "cds"
)
cat("KEY SET\n")


# Check Metadata/attributes for appropriate selection ---------------------

## List data sets available
df_cds_datasets <- wf_datasets(
  user = Sys.getenv("ECMWF_USER_UID"),
  service = "cds"
)

# get metdata of that specific data set ("seasonal-monthly-single-levels")
prod_info <- wf_product_info(
  dataset = "seasonal-monthly-single-levels",
  user = Sys.getenv("ECMWF_USER_UID"),
  service = "cds"
)


cat("defining bbox for extraction\n")
aoi_countries <- ne_countries(country = c("Nicaragua", "Honduras", "Guatemala", "El Salvador")) %>%
  st_as_sf() %>%
  select(
    contains("admin"),
    iso_a3
  )
aoi_bbox <- st_bbox(aoi_countries)


# Create API requests ---------------------------------------------------------
pub_mo_date <- format(floor_date(run_date, "month"), "%Y%m%d")

cat("writing data requests to list\n")
request_coords <- glue("{aoi_bbox['ymin']}/{aoi_bbox['xmin']}/{aoi_bbox['ymax']}/{aoi_bbox['xmax']}")

lr <- ecmwf_leadimes %>%
  map(\(int_lt){
    # valid_mo <- as_date(pub_mo_date,"%Y%m%d")+months(int_lt-1)
    bname <- paste0("lt", int_lt)
    fname_grib <- glue("ecmwf_seas51_monthly_{pub_mo_date}_lt{int_lt}.grib")
    ecmwf_data_request <- list(
      product_type = "monthly_mean",
      format = "grib",
      originating_centre = "ecmwf",
      system = "51",
      variable = c("total_precipitation"),
      year = as.character(year(run_date)),
      month = sprintf("%02d", month(run_date)),
      area = request_coords,
      leadtime_month = int_lt,
      dataset_short_name = "seasonal-monthly-single-levels",
      target = fname_grib
    )

    tmp_dir <- file.path(tempdir())
    wf_request(
      user = Sys.getenv("ECMWF_USER_UID"), # user ID (for authentication)
      request = ecmwf_data_request, # the request
      transfer = TRUE, # download the data
      path = tmp_dir
    )
    r_tmp <- rast(
      file.path(tmp_dir, fname_grib)
    )
    r_mean <- mean(r_tmp)
    r_mean %>%
      set.names(bname)
    return(r_mean)
  })

# merge bands
r <- rast(lr)


# make temp file
file_date_suffix <- format(floor_date(run_date, "month"))
fp_raster_name <- paste0("ecmwf_forecast_", file_date_suffix, "_aoi.tif")
tmp_path <- file.path(tempdir(), fp_raster_tmp)
writeRaster(r, tmp_path, overwrite = TRUE)


drive_upload(
  media = tmp_path,
  name = fp_raster_name,
  path = as_id(
    drive_dribble %>%
      filter(name == "ecmwf_seas51_monitoring_tifs") %>%
      pull(id)
  )
)

unlink(tmp_path)

lts <- parse_number(names(r))
