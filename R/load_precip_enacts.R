library(tidyverse)
library(terra)
library(RNetCDF)

#' load_precip_enacts
#' @description
#' Load ENACTS precipitation data from NetCDF file. ENACTS (Enhancing National
#' Climate Services) provides historical precipitation data for Guatemala.
#'
#' @param wrap `logical` whether to wrap the raster for targets pipeline storage (default TRUE)
#' @return terra SpatRaster object (wrapped if wrap=TRUE)
#' @examples \dontrun{
#' r_precip <- load_precip_enacts(wrap = FALSE)
#' }
load_precip_enacts <- function(wrap = TRUE) {
  fp_enacts <- file.path(
    Sys.getenv("AA_DATA_DIR_NEW"),
    "private",
    "raw",
    "lac",
    "INSIVUMEH",
    "ENACTS",
    "ENACTSv2.nc"
  )

  # Open the NetCDF file
  dat <- open.nc(fp_enacts)

  # Get coordinate variables
  lat <- var.get.nc(dat, "Y")
  lon <- var.get.nc(dat, "X")
  time_vals <- var.get.nc(dat, "T")


  # Calculate extent using cell centers
  # Use span/(n-1) to avoid floating point precision issues in stored coords
  dx <- (max(lon) - min(lon)) / (length(lon) - 1)
  dy <- (max(lat) - min(lat)) / (length(lat) - 1)
  dat_extent <- c(
    min(lon) - dx / 2, max(lon) + dx / 2,
    min(lat) - dy / 2, max(lat) + dy / 2
  )

  # Get precipitation values (variable name is "rfe" - rainfall estimate)
  precip_array <- var.get.nc(dat, "rfe")

  # Reorder dimensions for terra: (X,Y,T) -> (Y,X,T) i.e. (lon,lat,time) -> (lat,lon,time)
  precip_array_fixed <- aperm(precip_array, c(2, 1, 3))

  # Create raster
  r <- terra::rast(
    x = precip_array_fixed,
    ext = dat_extent,
    crs = "OGC:CRS84"
  )

  # Set band names based on time values
  # ENACTS time is days since 1981-01-01
  time_units <- att.get.nc(dat, "T", "units")
  time_origin <- as_date(str_extract(time_units, "\\d{4}-\\d{2}-\\d{2}"))
  dates <- time_origin + days(floor(time_vals))

  # Close the NetCDF connection
  close.nc(dat)

  names(r) <- as.character(dates)

  # Wrap for targets if requested
  if (wrap) {
    r <- wrap(r)
  }

  return(r)
}
