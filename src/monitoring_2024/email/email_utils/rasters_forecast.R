
#' Title
#' **NOTE** to reviewer: same as _targets function just chaned default wrap arg to F
#' @param gdb 
#' @param wrap 
#'
#' @return
#' @export
#'
#' @examples
load_ncdf_insivumeh <- function(gdb,wrap=F) {
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
zonal_insivumeh <- function(r, zone) {
  
  exact_extract(
    x = r,
    y = zone,
    fun = "mean"
  ) %>%
    pivot_longer(-matches("adm0_")) %>%
    separate(name, into = c("stat", "pub_date", "lt_chr"), sep = "\\.") %>%
    mutate(
      adm0_es = "Guatemala",
      pub_date = as_date(pub_date),
      lt = parse_number(lt_chr),
      valid_date = pub_date + months(lt)
    )
}


load_ecmwf_cog <- function(run_date,zone){
  run_date_chr <-  format(floor_date(run_date, "month"),"%Y%m")
  # from gdal documentation about vsi on s3
  url_raster <- paste0(
    "/vsis3/", # critical parameter
    Sys.getenv("BUCKET_NAME"),
    "/ECMWF_COGS/",
    run_date_chr,
    "-ECMWF_SEAS-V_i_a-ensemble_mean.tif"
  )
  
  # this downloads the selected AOI from global raster stored in S3 bucket
  fp_raster <- file.path(
    tempdir(),
    paste0(
      run_date_chr,
      "_ecmwf_seas.tif"
    )
  )
  sf::gdal_utils(
    "warp",
    source = url_raster,
    destination = fp_raster,
    options = c(
      "-t_srs", st_crs(zone)$wkt,
      "-te", sf::st_bbox(zone)
    )
  )
  
  ## 4b. Zonal Stats on ECMWF raster ####
  
  r_ecmwf <- terra::rast(
    fp_raster
  )
  return(r_ecmwf)
  
}

zonal_ecmwf <- function(r,zonal){
  
  exact_extract(x = r,
                y= zonal,
                fun="mean",
                append_cols="adm0_es"
  ) %>% 
    pivot_longer(-matches("adm0_")) %>%
    separate(name, into = c("stat", "pub_date", "lt"), sep = "\\.|_") %>%
    # add season information by combing dates w/ leadtimes
    mutate(
      pub_date = as_date(pub_date),
      lt = parse_number(lt),
      valid_date = pub_date + months(lt)
    )
}




