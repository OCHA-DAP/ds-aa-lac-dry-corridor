box::use(
dplyr[...],
tidyr[...],
stringr[...],
lubridate[...],
terra,
RNetCDF[...],
exactextractr[...]
)

#' load_precip_enacts
#' @description
#' Load ENACTS precipitation data from NetCDF file. ENACTS (Enhancing National
#' Climate Services) provides historical precipitation data for Guatemala.
#'
#' @param wrap `logical` whether to wrap the raster for targets pipeline storage (default TRUE)
#' @return terra SpatRaster object (wrapped if wrap=TRUE)
#' @export
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
  r <- terra$rast(
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
    r <- terra$wrap(r)
  }

  return(r)
}

#' monthly_zonal
#' @description
#' Calculate monthly zonal precipitation statistics from ENACTS daily raster.
#'
#' @param r terra SpatRaster with daily precipitation (band names as dates)
#' @param zone sf polygon to extract zonal statistics for
#' @param fun character aggregation function for exact_extract (default "mean")
#' @return data.frame with columns: date, year, month, value (monthly total precip in mm)
#' @export
#' @examples \dontrun{
#' r <- load_precip_enacts(wrap = FALSE)
#' gdf_aoi <- sf::st_read("path/to/aoi.gpkg")
#' df_monthly <- monthly_zonal(r, gdf_aoi)
#' }
monthly_zonal <- function(r, zone, fun = "mean") {
  # Extract daily zonal means
 df_daily <- exact_extract(
    x = r,
    y = zone,
    fun = fun,
    append_cols = FALSE
  )

  # Convert wide to long and parse dates from band names
 df_long <- df_daily |>
    pivot_longer(
      cols = everything(),
      names_to = "band",
      values_to = "value"
    ) |>
    mutate(
      # Remove "mean." prefix if present
      date = as_date(str_remove(band, "^mean\\.")),
      year = year(date),
      month = month(date)
    )

  # Aggregate to monthly totals
 df_monthly <- df_long |>
    group_by(year, month) |>
    summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      date = make_date(year, month, 1)
    ) |>
    arrange(date)

 return(df_monthly)
}
