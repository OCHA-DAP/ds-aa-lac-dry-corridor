#' Download ECMWF SEAS51 Data for the AOI (Nicaragua, Honduras, Guatemala, El Salvador)
#' Uses [ecmwfr package](https://github.com/bluegreen-labs/ecmwfr) package to access [ECMWF Climate Data Store API ](https://cds.climate.copernicus.eu/)
#' Registration instructions are available in teh above ecmwfr package link.

library(ecmwfr)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(glue)


fp_outpath_ecmwf <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "raw",
  "lac",
  "ecmwf_seasonal",
  "seas51"
  
)
# think only need to do this 1x
ecmwfr::wf_set_key(
  user=Sys.getenv("ECMWF_USER_UID"),
  key = Sys.getenv("ECMWF_USER_KEY"),
  service = "cds"
)


df_cds_datasets<- wf_datasets(
  user=Sys.getenv("ECMWF_USER_UID"),
  service = "cds"
)
df_cds_datasets %>% 
  tibble() %>% 
  filter(
    str_detect(name, "seas")
  )
prod_info <- wf_product_info(
  dataset = "seasonal-monthly-single-levels",
  user=Sys.getenv("ECMWF_USER_UID"),
  service = "cds"
)

cat("printing layers\n")
str_extract_all(prod_info$rich_abstract,pattern = "<td class='variables-name'>(.*?)</td>")


cat("defining bbox for extraction\n")
aoi_countries <- ne_countries(country = c("Nicaragua","Honduras","Guatemala","El Salvador")) %>% 
  st_as_sf() %>% 
  select(
    contains("admin"),
    iso_a3
  )
aoi_bbox <- st_bbox(aoi_countries) 


cat("writing data requests to list\n")
request_coords<- glue("{aoi_bbox['ymin']}/{aoi_bbox['xmin']}/{aoi_bbox['ymax']}/{aoi_bbox['xmax']}")


request_lte_2022 <- c(1:4) %>% 
  map(
    ~list(
      product_type = "monthly_mean",
      format = "netcdf",
      originating_centre = "ecmwf",
      system="51",
      variable = c("total_precipitation"),
      year = as.character(c(1981:2022)),
      month = sprintf("%02d",c(1:12)),
      area = request_coords,
      leadtime_month = .x,
      dataset_short_name = "seasonal-monthly-single-levels",
      target = glue("ecmwf_forecast_lte2022_lt{.x}.nc")
    )
  )

request_2023 <- c(1:4) %>% 
  map(
    ~list(
      product_type = "monthly_mean",
      format = "netcdf",
      originating_centre = "ecmwf",
      system ="51",
      variable = c("total_precipitation"),
      year = as.character(c(2023)),
      month = sprintf("%02d",c(1:9)),
      area = request_coords,
      leadtime_month = .x,
      dataset_short_name = "seasonal-monthly-single-levels",
      target = glue("ecmwf_forecast_2023_lt{.x}.nc")
    )
  )

cat("Downloading 2023 data\n")
request_2023 %>% 
  map(\(rq){
    wf_request(user     = Sys.getenv("ECMWF_USER_UID"),  # user ID (for authentication)
               request  = rq,  # the request
               transfer = TRUE,   # download the file
               path = fp_outpath_ecmwf)}
    )
cat("2023 data Jan - September downloaded to AA_DATA_DIR")

cat("Downloading data 1981-2022\n")
request_lte_2022 %>% 
  map(\(rq){
    wf_request(user     = Sys.getenv("ECMWF_USER_UID"),  # user ID (for authentication)
               request  = rq,  # the request
               transfer = TRUE,   # download the file
               path = fp_outpath_ecmwf)}
  )

cat('1981-2022 data Jan - December downloaded to AA_DATA_DIR')
