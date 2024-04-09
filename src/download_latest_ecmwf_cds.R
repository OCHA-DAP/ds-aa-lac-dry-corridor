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

testing_phase <-  T
ecmwf_leadimes <- c(1:6)

if(!testing_phase){
  run_date <- Sys.Date()
}
if(testing_phase){
  run_date <- as_date("2024-03-20")  
}

# pub dates - 5th day of every month
# https://www.ecmwf.int/en/newsletter/154/meteorology/ecmwfs-new-long-range-forecasting-system-seas5
fp_email_util_funcs <- list.files(
  here::here(file.path("src","email","email_utils")),
  full.names = T
)

walk(fp_email_util_funcs,~source(.x))
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


cat("defining bbox for extraction\n")
gdf_aoi <- load_drive_file(
  dribble = drive_dribble,
  file_name = "lac_cadc_adm0_no_islands.rds"
)
aoi_bbox <-  st_bbox(gdf_aoi)

# Create API requests ---------------------------------------------------------
pub_mo_date <- format(floor_date(run_date, "month"), "%Y%m%d")

cat("writing data requests to list\n")
# aoi_bbox <- round(aoi_bbox,0)
request_coords <- glue("{aoi_bbox['ymin']}/{aoi_bbox['xmin']}/{aoi_bbox['ymax']}/{aoi_bbox['xmax']}")

lr <- ecmwf_leadimes %>%
  map(\(int_lt){
    # valid_mo <- as_date(pub_mo_date,"%Y%m%d")+months(int_lt-1)
    bname <- paste0("lt", int_lt-1)
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
pub_mo <- floor_date(run_date, "month")

# transform from  avg m/hr to mm/month & harmonize band name
# we whats needed for analysis
lr_processed <- lr %>% 
  map(
    \(r_tmp){
      valid_mo <- pub_mo + months(parse_number(names(r_tmp)))
      mult_factor = days_in_month(valid_mo)*24*3600*1000
      r_tmp_mm <- r_tmp*mult_factor
      bname <-  paste0(pub_mo,".", names(r_tmp_mm))
      set.names(r_tmp_mm,bname) # in place
      return(r_tmp_mm)
    }
  )
r <- rast(lr_processed)

r_proj <- terra::project(r,"EPSG:4326")



# make temp file
file_date_suffix <- format(floor_date(run_date, "month"))
fp_raster_name <- paste0("cds_ecmwf_seas51_", file_date_suffix, "_aoi.tif")
tmp_path <- file.path(tempdir(), fp_raster_name)
writeRaster(r_proj, tmp_path, overwrite = TRUE)


drive_upload(
  media = tmp_path,
  name = fp_raster_name,
  path = as_id(
    drive_dribble %>%
      filter(name == "cds_ecmwf_seas51_tifs") %>%
      pull(id)
  ),
  overwrite = T
)

unlink(tmp_path)

# viz factory ideas putting in separate script

