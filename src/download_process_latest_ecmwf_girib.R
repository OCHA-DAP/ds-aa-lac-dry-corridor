library(tidyverse)
library(sf)
library(terra)
library(exactextractr)
library(here)
library(targets)
library(glue)
library(gghdx)
library(patchwork)
library(tidyterra) # may want to remove this dep from monitoring later.
library(aws.s3)
tar_source()
gghdx()

download_from_aws <- c(T,F)[1]

source(here("R/monitoring_aws.R"))
source(here("R/grib_tools.R"))
# source(here("R/monitoring_viz.R"))

tar_load(gdf_aoi_adm,store = here("_targets"))
# tar_load(r_ecmwf_mars,store=here("_targets"))
# targets::tar_load(df_mars_q_summary,store = here("_targets"))

run_date <-  Sys.Date()
run_mo <- format(run_date, "%Y-%m")





if(download_from_aws){
  bucket_df <- get_formatted_bucket_df()
  bucket_df_filt <- bucket_df %>% 
    filter(size_mb>12) %>% 
    filter(str_detect(date,run_mo))
  
  # However if we save the file to disk like this seems to work!
  lr <- map2(bucket_df_filt$Key,bucket_df_filt$filename ,
             \(keytmp,fntmp){
               fn <- file.path(
                 "ecmwf_gribs",
                 fntmp
               )
               
               save_object(bucket=Sys.getenv("BUCKET_NAME"),object = keytmp, file = fn,overwrite = T)
               r <- rast(fn) 
               return(r)
             }
  )
}

grib_dir <-  "ecmwf_gribs"
grib_files <- list.files(here(grib_dir),full.names = T)
grib_files_filt <-  str_subset(grib_files, pattern = run_mo)

if(!download_from_aws){
  lr <-  load_mf_ensemble_mean(file_paths =grib_files_filt )
  r <-  rast(lr)
}
# lr_ems <- load_mf_ensembles(file_paths = grib_files_filt)
# r_ems_nrt <- rast(lr_ems)




# load thresholds

# add folder to BUCKET
# aws.s3::put_folder(folder = "ECMWF_COGS",bucket = Sys.getenv("BUCKET_NAME"))

# official names from ECMWF Cataloge (for monthly ensemble means):
# "V-i-a: Seasonal 7-month forecast (SEAS) - Monthly means of ensemble means - Single level"

# Therefore I think this is a decent abbreviated capture:
pub_date <- floor_date(Sys.Date(),"month")
prefix_cog_name <-  format(pub_date,"%Y%m")
cog_name <- paste0(prefix_cog_name,"-ECMWF_SEAS-V_i_a-ensemble_mean.tif")
#> cog_name
#>"202402-ECMWF_SEAS-V_i_a-ensemble_mean.tif"


# write that to COG
terra::writeRaster(
  r, 
  filename = cog_name, 
  filetype = "COG",
  gdal = c("COMPRESS=DEFLATE", 
           "SPARSE_OK=YES",
           "OVERVIEW_RESAMPLING=AVERAGE")
)


# commented out, but this is the code upload global COG to AWS folder in bucket
aws.s3::put_object(file = cog_name,
                    object= file.path("ECMWF_COGS" ,
                                      cog_name),
                    bucket = Sys.getenv("BUCKET_NAME"))
# 
# bucket_df %>%  glimpse()



# 
# # Can I query these COGS smartly in the cloud?
# 
# # from gdal documentation about vsi on s3
# url_raster <- paste0(
#   "/vsis3/", # critical parameter
#   Sys.getenv("BUCKET_NAME"),
#   "/ECMWF_COGS/202402-ECMWF_SEAS-V_i_a-ensemble_mean.tif"
# )
# # this works... can also supply vsi arg here directly,
# # but not sure about clipping
# # r_dl <- terra::rast(
# #   url_raster
# #   )
# 
# 
# # this downloads the selected AOI from S3
# out_file <- "TEST_ECMWF_AOI.tif"
# sf::gdal_utils(
#   "warp",
#   source = url_raster,
#   destination = out_file,
#   options = c(
#     "-t_srs", sf::st_crs(gdf_aoi_adm$adm0)$wkt,
#     "-te", sf::st_bbox(gdf_aoi_adm$adm0)
#   )
# )
