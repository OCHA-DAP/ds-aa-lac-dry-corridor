#' Download ECMWF SEAS51 Data for the AOI (Nicaragua, Honduras, Guatemala, El Salvador)
#' Uses [ecmwfr package](https://github.com/bluegreen-labs/ecmwfr) package to access [ECMWF Climate Data Store API ](https://cds.climate.copernicus.eu/)
#' Registration instructions are available in teh above ecmwfr package link.

library(ecmwfr)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(glue)

download_historical_lte2022 <- T
download_2023 <-  F
download_leadtimes <-  1:6

# fp_outpath_ecmwf <- file.path(
#   Sys.getenv("AA_DATA_DIR"),
#   "private",
#   "raw",
#   "lac",
#   "ecmwf_seasonal",
#   "seas51",
#   "grib"
#   
# )
fp_outpath_ecmwf <- "test"
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

# filter data set table to those w/ seas
# find "seasonal-monthly-single-levels" dataset in list (and can explore others)
df_cds_datasets %>% 
  tibble() %>% 
  filter(
    str_detect(name, "seas")
  )

# get metdata of that specific data set ("seasonal-monthly-single-levels")
prod_info <- wf_product_info(
  dataset = "seasonal-monthly-single-levels",
  user=Sys.getenv("ECMWF_USER_UID"),
  service = "cds"
)

# rich_abstract has the most info, but hard to read so let's parse out the variable names like this
# this just for helping select correct variables
cat("printing layers\n")
str_extract_all(prod_info$rich_abstract,pattern = "<td class='variables-name'>(.*?)</td>")



# Define AOI ---------------------------------------------------------------
cat("defining bbox for extraction\n")
aoi_countries <- ne_countries(country = c("Nicaragua","Honduras","Guatemala","El Salvador")) %>% 
  st_as_sf() %>% 
  select(
    contains("admin"),
    iso_a3
  )
aoi_bbox <- st_bbox(aoi_countries) 


# Create API requests ---------------------------------------------------------


cat("writing data requests to list\n")
request_coords<- glue("{aoi_bbox['ymin']}/{aoi_bbox['xmin']}/{aoi_bbox['ymax']}/{aoi_bbox['xmax']}")


# Have to split current year from all historical data because API call fails if we enter a month
# that doesn't have data yet (i.e at the time of running - no data published 2023 October).
# it would be nice if API let you access the last month available programmatically - alas it does not
# there is a confluence issue somewhere that if I can find will share,

request_lte_2022 <- download_leadtimes %>% 
  map(
    ~list(
      product_type = "monthly_mean",
      format = "grib",
      originating_centre = "ecmwf",
      system="51",
      variable = c("total_precipitation"),
      year = as.character(c(1981:2022)),
      month = sprintf("%02d",c(1:12)),
      area = request_coords,
      leadtime_month = .x,
      dataset_short_name = "seasonal-monthly-single-levels",
      target = glue("ecmwf_forecast_lte2022_lt{.x}.grib")
    )
  )


request_2023 <- download_leadtimes %>% 
  walk(
    ~list(
      product_type = "monthly_mean",
      format = "grib",
      originating_centre = "ecmwf",
      system ="51",
      variable = c("total_precipitation"),
      year = as.character(c(2023)),
      month = sprintf("%02d",c(1:10)),
      area = request_coords,
      leadtime_month = .x,
      dataset_short_name = "seasonal-monthly-single-levels",
      target = glue("ecmwf_forecast_2023_lt{.x}.grib")
    )
  )

if(download_2023){
cat("Downloading 2023 data\n")
request_2023 %>% 
  walk(\(rq){
    wf_request(user     = Sys.getenv("ECMWF_USER_UID"),  # user ID (for authentication)
               request  = rq,  # the request
               transfer = TRUE,   # download the file
               path = fp_outpath_ecmwf)}
    )
cat("2023 data Jan - September downloaded to AA_DATA_DIR")
}

cat("Downloading data 1981-2022\n")
if(download_historical_lte2022){
request_lte_2022 %>% 
  walk(\(rq){
    wf_request(user     = Sys.getenv("ECMWF_USER_UID"),  # user ID (for authentication)
               request  = rq,  # the request
               transfer = TRUE,   # download the file
               path = fp_outpath_ecmwf)}
  )
}

cat('1981-2022 data Jan - December downloaded to AA_DATA_DIR')
