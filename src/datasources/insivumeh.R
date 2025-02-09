box::use(
  dplyr[...],
  purrr[...],
  glue,
  RNetCDF,
  ncmeta,
  lubridate[...],
  stringr[...],
  AzureStor,
  terra,
  cumulus,
  tidyr,
  readr,
  exactextractr
  
)

#' @export
load_zonal_insivumeh <-  function(run_date = Sys.Date(),
                                  zone,
                                  container = "global",
                                  dir = "raster/raw",
                                  stage = "dev"){

  # Define and create temporary directory
  td <- file.path(tempdir(),"insivumeh")
  # dir.create(td, showWarnings = FALSE)
  
  r_insiv <- load_ncdf_blob_insiv(run_date = run_date, temp_dir = td)
  
  # Load raster into memory to prevent issues after deletion
  # r_insiv <- terra::wrap(r_insiv)
  
  logger$log_info("Running INSIVUMEH Zonal Stats")
  dfz_insiv <- zonal_insivumeh(
    r = r_insiv,
    zone = zone
  ) |> 
    mutate(
      iso3 ="GTM",
      forecast_source ="INSIVUMEH"
    )
  # Ensure deletion after function exits
  on.exit({
    unlink(td, recursive = TRUE, force = TRUE)
    message("Temporary directory deleted: ", td)
  }, add = TRUE)
  return(dfz_insiv)

}
#' @export
load_ncdf_blob_insiv <- function(
    run_date = Sys.Date(),
    container = "global",
    dir = "raster/raw",
    stage = "dev"
    ){
  
  run_mo = month(run_date,label = TRUE, abbr = TRUE)
  run_yr = year(run_date)
  
  prefix<- paste0("insivumeh_pronos_deterministic")
  suffix<- paste0("start",run_mo,run_yr,".nc")
  rgx <- glue$glue("^{dir}/{prefix}.*{suffix}$")
  
  blob_container <- cumulus$blob_containers()$global
  

  blob_contents <- AzureStor$list_blobs(container = blob_container,dir = "raster/raw")
  blob_names <- stringr::str_subset(blob_contents$name,pattern = rgx)
  # Check if blob_names is empty
  if (length(blob_names) == 0) {
    stop("No matching files found for the specified criteria. Check the run_date, container, and dir arguments.")
   }
  td <- file.path(tempdir(),"insivumeh")
  dir.create(td)
  
  # Ensure the temporary directory is deleted when the function exits -- 

  
  blob_names |> 
    purrr::map(
      \(b){
        
        tf <- file.path(td,basename(b))
        
        AzureStor$download_blob(
          container = blob_container,
          src = b,
          dest = tf,
          overwrite = TRUE
        )
        
      }
    )
  r <- load_ncdf_insivumeh(gdb = td)
  on.exit({
    unlink(td, recursive = TRUE, force = TRUE)
    message("Temporary directory deleted: ", td)
  }, add = TRUE)
  return(r)

}

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
  
  lr <- fps |> 
    map(\(fp_nc){
      # meta data collection  - to write band_name as "{pub_date}.lt_{leadtime}"
      valid_time_meta <- ncmeta$nc_att(fp_nc, "T", "units")$value$units
      start_time_meta <- ncmeta$nc_att(fp_nc, "S", "units")$value$units
      valid_date <- as_date(str_extract(valid_time_meta, "\\d{4}-\\d{2}-\\d{2}"))
      start_date <- as_date(str_extract(start_time_meta, "\\d{4}-\\d{2}-\\d{2}"))
      lead_time_diff <- interval(start_date, valid_date)
      lt <- lead_time_diff %/% months(1)
      bname <- paste0(start_date, ".lt_", lt)
      
      # Now we open the each file and turn it into a terra::rast()
      cat(bname, "\n")
      dat <- RNetCDF$open.nc(fp_nc)
      dat_extent <- r_extent(nc_ob= dat)
      value_array <-  RNetCDF$var.get.nc(dat, "deterministic")
      value_array_fixed <- aperm(value_array, c(2, 1))
      
      rtmp <- terra$rast(
        x = value_array_fixed,
        ext = dat_extent,
        crs = "OGC:CRS84"
      )
      # set correct band nanme
      rtmp %>%
        terra$set.names(bname)
      return(rtmp)
    })
  # merge multi-band
  r <- terra$rast(lr)
  
  # wrap so it can be saved as a target - in monitoring we won't want it wrapped
  if(wrap){
    r <- terra$wrap(r)  
  }
  return(r)
}



#' Title
#'
#' @param nc_ob 
#'
#' @return
#'
#' @examples
#' nc_ck<- RNetCDF::open.nc(fps[1])
#' r_extent(nc_ck  )

r_extent <- function(nc_ob){
  lat <- RNetCDF$var.get.nc(nc_ob, "Y")
  lon <- RNetCDF$var.get.nc(nc_ob, "X")
  dx <- diff(lon[1:2])
  dy <- abs(diff(lat[1:2]))
  ex <- c(min(lon) - dx/2, max(lon) + dx/2,
          min(lat) - dy/2, max(lat) + dy/2)
  return(ex)
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
#' @export
  zonal_insivumeh <- function(r, zone) {
    
    exactextractr$exact_extract(
      x = r,
      y = zone,
      fun = "mean"
    ) %>%
      tidyr$pivot_longer(everything()) %>%
      tidyr$separate(name, into = c("stat", "issued_date", "lt_chr"), sep = "\\.") %>%
      mutate(
        adm0_es = "Guatemala",
        issued_date = as_date(issued_date),
        leadtime = readr$parse_number(lt_chr),
        valid_date = issued_date + months(leadtime)
      ) |> 
      filter(
        month(valid_date)%in% 5:11
      ) |> 
      filter(
        month(issued_date)!=2
      ) 
    
      
  }


#' @export
insivumeh_availability <- function(run_date){
  # INSIVUMEH Forecast currenlty stored in global container on dev
  # when I set it up i thought we were goign to put all rasters in one location
  # and use STAC for cataloguing
  blob_container <- cumulus$blob_containers()$global 
  DIR_CURRENT_INSIV <- paste0("start",month(run_date,abbr = T,label = T))
  blob_name_rgx <- paste0("start",format(run_date,"%b%Y"),".nc$")
  
  
  container_contents <- AzureStor$list_blobs(
    blob_container, 
    dir = "raster/raw"
  )
  fps_blob <- str_subset(container_contents$name,blob_name_rgx)
  cat("checking global/raster/raw for 6 new forecast files\n")
  filenames_unique <-  unique(fps_blob)
  num_unique_files <- length(fps_blob)
  
  ret_lgl <- num_unique_files==6
  if(ret_lgl){
    cat("6 unique INSIVUMEH files found for current run month")
  }else{
    cat("6 unique INSIVUMEH files NOT FOUND for current month")
  }
  ret_lgl
}



