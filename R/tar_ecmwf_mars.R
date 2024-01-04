#' zonal_ecmwf_mars
#'
#' @param r_wrapped
#' @param zone polygon of zones
#' @param stat `character` stat to reduce raster to over zones
#'
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
#' zonal_ecmwf_mars(r_wrapped = r, zone = gdf_aoi_adm$adm0, stat = "mean")
#' }
zonal_ecmwf_mars <- function(r_wrapped = r_ecmwf_mars, zone = gdf_aoi_adm$adm0, stat = "mean") {
  r <- unwrap(r_wrapped)
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
#' @param gdb `character` filepath to ecmwf tifs
#' @note
#' I have had issues w/ this concept of wrapping rasters in targets before,
#' but this seems to be working as is here. For some reason in previous work I've had to write out the raster
#' to a file path and then wrap from there. If something goes wrong i'd like to try document the best solution,
#' @return wrapped spatraster object
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
#' load_mars_raster(gdb = gdb_ecmwf_mars_tifs)
#' }
load_mars_raster <- function(gdb = gdb_ecmwf_mars_tifs) {
  rm_name <- "lac_seasonal-montly-individual-members_tprate-"
  fps <- list.files(
    path = gdb, pattern = "\\.tif$",
    full.names = T
  )
  r <- rast(fps)
  names(r) <- str_remove(names(r), rm_name)
  return(wrap(r))
}
