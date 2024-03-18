#' This used to be part of the `_targets.R` pipeline, but I've decided to download the
#' files to the gdrive so that the pipeline runs off a static file that reflects the CODs at the
#' time of analysis. Updates to the CODs breaking this target is annoying because alot of the
#' heavier processes rely on it.

library(tidyverse)
library(sf)
library(rhdx)
library(targets) # for tar_source()
tar_source()

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


gha_dir <- fs::path(
  Sys.getenv("AA_DATA_DIR"), 
  "..",
  "..",
  "ROLAC",
  "Dry Corridor",
  "CADC_GHA" 
  )


lgdf$adm0 %>% 
  write_rds(
    file.path(
      gha_dir, 
      "central_america_aoi_adm0.rds"
    )
    
  )

adm0_simp <- st_simplify(gdf_aoi_adm$adm0,dTolerance = 100)


adm1_simp <- st_simplify(gdf_aoi_adm$adm1,dTolerance= 10)
adm1_simp <- adm1_simp %>% 
  mutate(
    area = as.numeric(st_area(.))
  ) %>% 
  filter(
    area>3e8
  )
  

# ggplot(aes(x= area))+
#   geom_histogram()+
#   scale_x_log10()
# adm1_simp %>% 
#   object.size() *1e-6
# plot(adm1_simp$geometry,add=T)

adm0_simp %>% 
  write_rds(
    file.path(
      gha_dir, 
      "central_america_aoi_adm0_simplified.rds"
    )
    
  )
adm1_simp %>% 
  write_rds(
    file.path(
      gha_dir, 
      "central_america_aoi_adm1_simplified.rds"
    )
    
  )

# played around w/ sfarrow for writing geoparquets to try to make it lighter
# but only skimmed about 0.5 mb and got a warning saying sfarrow imlemetnation "does not yet make stability promises"


# let's get panama and Mexico for mapping purposes
gdf_pan_adm0 <- search_datasets("Panama - Subnational Administrative Boundaries") %>% 
  pluck(1) %>% 
  get_resource(2) %>% 
  read_resource(layer= "pan_admbnda_adm0_gadm_20211020")

gdf_mex_adm0 <- search_datasets("Mexico - Subnational Administrative Boundaries") %>% 
  pluck(1) %>% 
  get_resource(4) %>%
  # get_resource_layers()
  read_resource(layer= "mex_admbnda_adm0_govmex_20210618")
gdf_costa_adm0 <- search_datasets("Costa Rica - Subnational Administrative Boundaries") %>% 
  pluck(2) %>% 
  get_resource(4) %>%
  # get_resource_layers()
  read_resource(layer= "geoBoundaries-CRI-ADM0")

gdf_belize_adm0 <- search_datasets("Belize - Subnational Administrative Boundaries") %>% 
  pluck(1) %>% 
  get_resource(4) %>% # 2 broken in API
  # get_resource_layers()
  read_resource(layer= "blz_admbnda_adm0_gadm_20210910" )


gdf_adm0_surrounding <- bind_rows(
  gdf_pan_adm0 %>% 
    clean_names() %>% 
    select(matches("^adm\\d_[ep]")),
  gdf_mex_adm0 %>% 
    rename(
      adm0_es = "admin0Name_es",
      adm0_pcode = "admin0Pcode",
      geometry = "Shape"
    ) %>% 
    select(matches("^adm\\d_[ep]")),
  gdf_costa_adm0 %>% 
    select(
      adm0_es = "shapeName",
      adm0_pcode = "shapeISO"
    ),
  gdf_belize_adm0 %>% 
    select(
      adm0_es = "admin0Name_en",
      adm0_pcode = "admin0Pcode",
      geometry = "Shape"
    )
)

gdf_adm0_surrounding %>% 
  object.size() *1e-6

gdf_adm0_surrounding_simp <- st_simplify(gdf_adm0_surrounding,dTolerance = 100)

gdf_adm0_surrounding_simp %>% 
  object.size() *1e-6

gdf_adm0_surrounding_simp %>% 
  write_rds(
    file.path(
      gha_dir, 
      "surrounding_aoi_adm0_simplified.rds"
    )
    
  )


  
