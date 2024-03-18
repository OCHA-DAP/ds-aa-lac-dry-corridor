#' Make a simplified shape file for the 4 countries in the Central American 
#' Dry Corridor. The original shapefile was made in targets. However, to improve
#' interopoerabilitiy between python and R we will just write this out to a 
#' shapefile. For our purposes it is simplifeid with tiny polygons removed

library(targets)
library(sf)
library(tidvyverse)

tar_load(gdf_aoi_adm)

DIR_ADM0_PROC <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public","processed","lac"
  )
FP_ADM0_PROC <-  file.path(
  DIR_ADM0_PROC,
  "adm0_central_ameria_dry_corridor_simp.shp"
  )

             
# lets simplify admin 1 - remove slivers/islands
adm0_main <- gdf_aoi_adm$adm0 %>%
  st_cast("POLYGON") %>%
  mutate(
    area = as.numeric(st_area(.))
  ) %>%
  filter(
    area > 1e10
  )



st_write(adm0_main,FP_ADM0_PROC)

