library(tidyverse)
library(terra)
library(sf)
library(exactextractr)

#' zonal_ecmwf_mars
#' @description
#' Function to run zonal stats on raster and return long data frame with zonal stat
#' this function actually works on both ecmwf CD and ecmwf MARS after they have been
#' processed in to monthly values in the _targets pipeline
#' 
#' Function is not currently generic and dependent on required columns/raster data structure
#'
#' @param r_wrapped Wrapped SpatRaster object return by `terra::wrap()`
#' @param zone polygon of zones
#' @param stat `character` stat to reduce raster to over zones
#' @return data.frame
#' @examples \dontrun{
#' library(tidyverse)
#' library(targets)
#' tar_source() # source `R/`
#'
#' # load adm boundaries
#' tar_load(gdf_aoi_adm)
#' gdb_ecmwf_mars_tifs <- file.path(
#'   Sys.getenv("AA_DATA_DIR"),
#'   "private",
#'   "processed",
#'   "lac",
#'   "ecmwf_seasonal",
#'   "seas51",
#'   "mars"
#' )
#' r <- load_mars_raster(gdb = gdb_ecmwf_mars_tifs)
#' zonal_ecmwf(r_wrapped = r, zone = gdf_aoi_adm$adm0, stat = "mean")
#' }
zonal_ecmwf <- function(r_wrapped = r_ecmwf_mars, zone = gdf_aoi_adm$adm0, stat = "mean") {
  r <- terra::unwrap(r_wrapped)
  exact_extract(
    x = r,
    y = zone,
    fun = stat,
    append_cols = c("adm0_es", "adm0_pcode")
  ) %>%
    pivot_longer(-matches("adm0_")) %>%
    separate(name, into = c("stat", "pub_date", "lt"), sep = "\\.|_") %>%
    # add season information by combing dates w/ leadtimes
    mutate(
      pub_date = as_date(pub_date),
      lt = parse_number(lt),
      lt = lt - 1,
      valid_date = pub_date + months(lt)
    )
}

#' load_mars_raster
#'
#' @param gdb `character` filepath to ecmwf CDs tifs
#' @param rm_from_ly_name `character` if file name read in and concatenated to band we can use this arg to remove it
#' @param wrap `logical` (default=F) whether or not to terra::wrap() final raster object. In _targets pipeline its useful to wrap it.
#' @note
#' I have had issues w/ this concept of wrapping rasters in targets before,
#' but this seems to be working as is here. For some reason in previous work I've had to write out the raster
#' to a file path and then wrap from there. If something goes wrong i'd like to try document the best solution,
#' @return SpatRaster or Wrapped SpatRaster object
#' @examples \dontrun{
#' library(tidyverse)
#' library(targets)
#' tar_source
#'
#' gdb_ecmwf_mars_tifs <- file.path(
#'   Sys.getenv("AA_DATA_DIR"),
#'   "private",
#'   "processed",
#'   "lac",
#'   "ecmwf_seasonal",
#'   "seas51",
#'   "mars"
#' )
#' load_ecmwf_mars_stack(gdb = gdb_ecmwf_mars_tifs)
#' }
load_ecmwf_mars_stack <- function(gdb = gdb_ecmwf_cds_tifs,
                                  rm_from_ly_name="lac_seasonal-montly-individual-members_tprate-",
                                  wrap=F) {
  
  fps <- list.files(
    path = gdb, pattern = "\\.tif$",
    full.names = T
  )
  r <- rast(fps)
  names(r) <- str_remove(names(r), rm_from_ly_name)
  
  if(wrap){
    r <- terra::wrap(r)
  }
  return(r)
}

#' @description
#' func to load CD raster tif files from single folder
#' differs from above function as band name and data structure is slightly different
#' as rasters were processed at different stages of project by different analysts.
#' This was actually done first before MARS.
#' @param gdb `character` filepath to ecmwf MARS tifs
#' @param wrap `logical` (default=F) whether or not to terra::wrap() final raster object. In _targets pipeline its useful to wrap it.
#' @return SpatRaster or Wrapped SpatRaster object

load_ecmwf_cd_stack <-  function(gdb,wrap=F){
  fps <- list.files(
    path = gdb, pattern = "\\.tif$",
    full.names = T
  )
  lr <- fps %>% 
    map(\(fp_temp){
      fp_temp_base <- basename(fp_temp)
      pub_date <-  as_date(str_extract(fp_temp_base,"\\d{4}-\\d{2}-\\d{2}"))
      rtmp <- rast(fp_temp)
      
      bnames <- paste0(pub_date,"_",names(rtmp))
      rtmp %>% 
        set.names(bnames)
      return(rtmp)
    })
  r_ret <- rast(lr)
  if(wrap){
    r_ret <- terra::wrap(r_ret)
  }
  return(r_ret)
}