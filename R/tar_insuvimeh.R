library(tidyverse)
library(terra)
library(sf)

#' catalogue_insuvimeh_files
#' @description
#' Provide the folder directory path to the insuvimeh files and return data.frame containing meta
#' data to each file based on `n` file attributes and file/folder name structure
#'
#' **note** non-generic function specific to this pipeline and the way we are currently receiving files from INSIVUMEH
#'
#' @param gdb `character` file path to parent folder containing all INSIVUMEH forecasts of interest.
#' @param file_name_pattern `character` regex to correctly identify all files to be considered (default ='\\d{4}.nc$')
#' @return data.frame with containing metadata on all files based on `nc` file attributes
#' @examples \dontrun{
#'
#' }
catalogue_insuvimeh_files <- function(gdb, file_name_pattern = "\\d{4}.nc$") {
  fps <- list.files(gdb,
                    full.names = T,
                    recursive = T,
                    pattern = file_name_pattern
  )
  
  fps_w_parents <- list.files(gdb,
                              recursive = T,
                              pattern = file_name_pattern
  )
  lr <- fps %>%
    map2(fps_w_parents, \(fp_nc, fp_par){
      fp_base <- basename(fp_nc)
      # meta data collection  - to write band_name as "{pub_date}.lt_{leadtime}"
      valid_time_meta <- ncmeta::nc_att(fp_nc, "T", "units")$value$units
      start_time_meta <- ncmeta::nc_att(fp_nc, "S", "units")$value$units
      valid_date <- as_date(str_extract(valid_time_meta, "\\d{4}-\\d{2}-\\d{2}"))
      start_date <- as_date(str_extract(start_time_meta, "\\d{4}-\\d{2}-\\d{2}"))
      lead_time_diff <- interval(start_date, valid_date)
      lt <- lead_time_diff %/% months(1)
      tibble(
        file_name_parent = fp_par,
        file_name = fp_base,
        pub_date = start_date,
        valid_date = valid_date,
        lt = lt
      )
    }) %>%
    list_rbind()
  return(lr)
}



#' load_insuvimeh_raster2
#' An improved version of load raster where we utilize lessons learned
#' from FloodScan to better set extent using r_extent() function defined below
#' **TODO* rm fps[1:3] for actual run.... re-run thresholds to see if any difference
#' worth re-running for postrera.
#'
#' @param gdb 
#' @param wrap 
#'
#' @return
#' @export
#'
#' @examples
#' load_insuvimeh_raster2(gdb = gdb,wrap=FALSE)
load_insuvimeh_raster2 <- function(gdb,wrap=T) {
  fps <- list.files(gdb,
                    full.names = T,
                    recursive = T,
                    pattern = "\\d{4}.nc$"
  )
  
  ## NOTE CHANGE THIS ON FULL RUN
  # fps <- fps[1:3]
  lr <- fps %>%
    map(\(fp_nc){
      # meta data collection  - to write band_name as "{pub_date}.lt_{leadtime}"
      valid_time_meta <- ncmeta::nc_att(fp_nc, "T", "units")$value$units
      start_time_meta <- ncmeta::nc_att(fp_nc, "S", "units")$value$units
      valid_date <- as_date(str_extract(valid_time_meta, "\\d{4}-\\d{2}-\\d{2}"))
      start_date <- as_date(str_extract(start_time_meta, "\\d{4}-\\d{2}-\\d{2}"))
      lead_time_diff <- interval(start_date, valid_date)
      lt <- lead_time_diff %/% months(1)
      bname <- paste0(start_date, ".lt_", lt)
      
      # Now we open the each file and turn it into a terra::rast()
      cat(bname, "\n")
      dat <- RNetCDF::open.nc(fp_nc)
      dat_extent <- r_extent(nc_ob= dat)
      value_array <-  var.get.nc(dat, "deterministic")
      value_array_fixed <- aperm(value_array, c(2, 1))
      
      rtmp <- terra::rast(
        x = value_array_fixed,
        ext = dat_extent,
        crs = "OGC:CRS84"
        # crs = "EPSG:4326"
      )
      # set correct band nanme
      rtmp %>%
        set.names(bname)
      return(rtmp)
    })
  # merge multi-band
  r <- rast(lr)
  
  # wrap so it can be saved as a target - in monitoring we won't want it wrapped
  if(wrap){
    r <- wrap(r)  
  }
  return(r)
}



#' Title
#'
#' @param nc_ob 
#'
#' @return
#' @export
#'
#' @examples
#' nc_ck<- RNetCDF::open.nc(fps[1])
#' r_extent(nc_ck  )

r_extent <- function(nc_ob){
  lat <- var.get.nc(nc_ob, "Y")
  lon <- var.get.nc(nc_ob, "X")
  dx <- diff(lon[1:2])
  dy <- abs(diff(lat[1:2]))
  ex <- c(min(lon) - dx/2, max(lon) + dx/2,
          min(lat) - dy/2, max(lat) + dy/2)
  return(ex)
}


load_insuvimeh_raster <- function(gdb,wrap=T) {
  fps <- list.files(gdb,
                    full.names = T,
                    recursive = T,
                    pattern = "\\d{4}.nc$"
  )
  
  lr <- fps %>%
    map(\(fp_nc){
      # meta data collection  - to write band_name as "{pub_date}.lt_{leadtime}"
      valid_time_meta <- ncmeta::nc_att(fp_nc, "T", "units")$value$units
      start_time_meta <- ncmeta::nc_att(fp_nc, "S", "units")$value$units
      valid_date <- as_date(str_extract(valid_time_meta, "\\d{4}-\\d{2}-\\d{2}"))
      start_date <- as_date(str_extract(start_time_meta, "\\d{4}-\\d{2}-\\d{2}"))
      lead_time_diff <- interval(start_date, valid_date)
      lt <- lead_time_diff %/% months(1)
      bname <- paste0(start_date, ".lt_", lt)
      
      # Now we open the each file and turn it into a terra::rast()
      cat(bname, "\n")
      dat <- ncdf4::nc_open(fp_nc)
      lon <- ncdf4::ncvar_get(dat, "X")
      lat <- ncdf4::ncvar_get(dat, "Y")
      value_array <- ncdf4::ncvar_get(dat, "deterministic")
      value_array_fixed <- aperm(value_array, c(2, 1))
      
      rtmp <- terra::rast(
        x = value_array_fixed,
        extent = ext(
          min(lon),
          max(lon),
          min(lat),
          max(lat)
        ),
        crs = "OGC:CRS84"
      )
      # set correct band nanme
      rtmp %>%
        set.names(bname)
      return(rtmp)
    })
  # merge multi-band
  r <- rast(lr)
  
  # wrap so it can be saved as a target - in monitoring we won't want it wrapped
  if(wrap){
    r <- wrap(r)  
  }
  return(r)
}

#' zonal_gtm_insuvimeh
#' @description
#' custom targets function to run zonal means by leadtime and publication date for forecast provided by insivumeh
#'
#' @param r `packedSpatRaster` terra object created in pipeline
#' @param gdf sf class polygon to run zonal statistics to
#' @param rm_dup_years `logical` just parameter used in beginning when catalogue had issues
#'
#' @return `data.frame` in long format w/ zonal means by publication date and leadtime
zonal_gtm_insuvimeh <- function(r, gdf, rm_dup_years = F) {
  r_gtm <- unwrap(r)
  if (rm_dup_years) {
    r_nm_yr_gtm <- as_date(str_extract(names(r_gtm), "\\d{4}-\\d{2}-\\d{2}"))
    r_gtm <- r_gtm[[year(r_nm_yr_gtm) > 1982]] # these years have duplicate folders
  }
  
  gdf_gtm_adm <- gdf %>%
    map(
      ~ .x %>%
        filter(
          adm0_es == "Guatemala"
        )
    )
  exact_extract(
    x = r_gtm,
    y = gdf_gtm_adm$adm0,
    fun = "mean"
  ) %>%
    pivot_longer(everything()) %>%
    separate(name, into = c("stat", "pub_date", "lt_chr"), sep = "\\.") %>%
    mutate(
      adm0_es = "Guatemala",
      pub_date = as_date(pub_date),
      lt = parse_number(lt_chr),
      valid_date = pub_date + months(lt)
    )
}
