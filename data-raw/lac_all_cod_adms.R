#' This used to be part of the `_targets.R` pipeline, but I've decided to download the
#' files to the gdrive so that the pipeline runs off a static file that reflects the CODs at the
#' time of analysis. Updates to the CODs breaking this target is annoying because alot of the
#' heavier processes rely on it.

library(tidyverse)
library(sf)
library(rhdx)

# func in R folder that load the sf objectgs into alist
lgdf<- load_proj_admins() %>%
  map(~ .x %>%
        st_make_valid() %>%
        select(matches("^adm\\d_[ep]")))

lgdf %>%
  write_rds(
    file.path(
      Sys.getenv("AA_DATA_DIR"),
      "public",
      "processed",
      "lac",
      "lac_all_cod_adms.rds"
    )
  )


# realize historical CD data is clipped to simplified Shapefile where some of the small caribbean islands are removed
# therefore to standardize we will remove this from the non-simplifeid version and use that for some zonal stats


gdf_adms_full <- read_rds(
  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public",
    "processed",
    "lac",
    "lac_all_cod_adms.rds"
  )
)
gdf_adm0_full <-  gdf_adms_full$adm0  

gdf_adm0_full_area <- gdf_adm0_full %>% 
  st_cast("POLYGON") %>% 
  mutate(
    area= as.numeric(st_area(.))
  ) 

gdf_adm0_full_area %>% 
  st_drop_geometry() %>% 
  ggplot(
    aes(x= area)
  )+
  geom_histogram()+
  scale_x_log10(labels=scales::label_comma())

gdf_adm0_poly_no_islands <- gdf_adm0_full_area %>% 
  filter(
    area > 1e10
  ) 

#before
plot(gdf_adm0_full$geometry)

# after
gdf_adm0_poly_no_islands$geometry %>% plot()

fp_lac_adm0_no_islands <-   file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public",
  "processed",
  "lac",
  "lac_cadc_adm0_no_islands.rds"
)
write_rds(
  gdf_adm0_poly_no_islands,
  fp_lac_adm0_no_islands
)



# add file to drive
library(googledrive)
gdf_adm0_poly_no_islands <- read_rds(
    file.path(
      Sys.getenv("AA_DATA_DIR"),
      "public",
      "processed",
      "lac",
      "lac_cadc_adm0_no_islands.rds"
    )
  )


# authorize drive access
drive_auth(
  path = Sys.getenv("CADC_MONITORING_JSON")
)

drive_dribble <- drive_ls(
  corpus = "user"
)

folder_id<- drive_dribble %>% 
  filter(
    name == "CADC_GHA"
  ) %>% 
  pull(id)

drive_upload(
  media = fp_lac_adm0_no_islands,
  name = basename(fp_lac_adm0_no_islands),
  path = as_id(folder_id)
)
  