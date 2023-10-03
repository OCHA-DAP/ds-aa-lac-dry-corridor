
# VHI Monthly -------------------------------------------------------------

## Description ####

# Download monthly VHI Rasters from FAO.
# Zonal stats by admin CODs was taking too long in GEE so we download the rasters (clipped to AOI BBOX) locally


## Requirements ####
# you will need a GEE account and `{rgee}` set up (https://github.com/r-spatial/rgee)
# You will also need access to FAO ASIS user group.
# **Note** code reviewer will likely not have access- so they can just access the downloaded data directly on the gdrive


## Details/note ###
# script meant to be run as a background process through terminal
# i.e on mac I do `caffeinate -s Rscript data-raw/vhi_monthly_raster.R`
# You may want to run `ee_Initialize()` out of background processing if you face an authentication error



## Implementation ####

### Libraries ####
library(tidyverse)
library(sf)
library(targets)
library(rgee)
library(tidyrgee)

tar_load(gdf_aoi_adm) # admin AOI from `_targets`

# authenticate
ee_Initialize()

# monthly VHI GEE URL
ic_mo_url="projects/UNFAO/ASIS/VHI_M"




# convert to bbox then to ee geom
aoi_ee <- gdf_aoi_adm$adm0 %>%
  st_bbox() %>% 
  st_as_sfc() %>% 
  sf_as_ee()

ic_mo <- ee$ImageCollection(ic_mo_url)

# crop to bbox
ic_mo_filt <- ic_mo$filterBounds(aoi_ee)

# downoad all monthly data
tmp <- tempdir()
ic_drive_files_mo <- ee_imagecollection_to_local(
  ic = ic_mo_filt,
  region = aoi_ee,
  scale = 1000,
  via = "drive",
  timePrefix = F,
  lazy = TRUE,
  dsn =file.path(tmp)
)


# somehow got some weird file names to correct after using rgee to download
gee_download_dir <- 
  file.path("data","rgee_backup")
  
out_dir <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "raw",
  "lac",
  "vhi_fao"
)
list.files(gee_download_dir,full.names = T) %>% 
  map(
    \(fptmp){
      fptmp_base <-  basename(fptmp)
      cat(fptmp_base,"\n")
      fptmp_base_new <- str_replace(fptmp_base, 
                                    "RtmpHtuvfr",
                                    "FAO_slv_hnd_nic_gtm_")
      fp_new<- file.path(
        out_dir,fptmp_base_new
      )
      file.copy(fptmp,to = fp_new)
    }
  )

