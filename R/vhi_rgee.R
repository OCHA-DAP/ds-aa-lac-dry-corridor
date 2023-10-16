#' **NOTE: NOT USED** - These functions not used so far in repo
#' They were originally designed to process the VHI in GEE with `{rgee}`
#' However, the complexity and errors in the zonal admin layers (NIC,HND,GTM,ESV)
#' Make them to costly in-terms of time required to run the final zonal statistics
#' Therefore, we opted to just download the rasters locally (see `data-raw/vhi_monthly_rasters.R`) and speed
#' up computations w/ exactextractr.  However, I am choosing to keep these functions for now in case
#' they become useful later in this project or others.



#' vhi_mo_composite_reduce_region
#'
#' @param region
#' @param ic_url
#' @param moi
#' @param stat
#' @param scale
#' @param via
#'
#' @return
#' @export
#'
#' @examples
vhi_mo_composite_reduce_region <- function(region,
                                           ic_url = "projects/UNFAO/ASIS/VHI_D",
                                           moi = 3:11,
                                           stat = list("mean", "median"),
                                           scale = 1000,
                                           via = "drive") {
  ic_monthly <- vhi_year_month_composites(
    ic_url = ic_url,
    moi = moi,
    stat = stat
  )
  ic_reduced <- ic_monthly %>%
    ee_extract_tidy(
      y = region,
      scale = 1000,
      via = via
    )
  return(ic_reduced)
}
vhi_mo_composite_reduce_region_batch <- function(region,
                                                 ic_url = "projects/UNFAO/ASIS/VHI_D",
                                                 moi = 3:11,
                                                 stat = list("mean", "median"),
                                                 scale = 1000,
                                                 via = "drive") {
  ic_monthly <- vhi_year_month_composites(
    ic_url = ic_url,
    moi = moi,
    stat = stat
  )

  years_unique <- ic_monthly$vrt$year %>%
    unique() %>%
    sort()
  ic_reduced <- years_unique %>%
    map(
      \(yrtmp){
        cat("Running stats on year: ", yrtmp, "\n")
        ic_monthly %>%
          filter(year == yrtmp) %>%
          ee_extract_tidy(
            y = region,
            scale = 1000,
            via = via
          )
      }
    )
  return(ic_reduced)
}
#' vhi_year_month_composites
#'
#' @param ic_url
#' @param moi
#' @param stat
#'
#' @return
#' @export
#'
#' @examples
vhi_year_month_composites <- function(
    ic_url = "projects/UNFAO/ASIS/VHI_D",
    moi = 3:11,
    stat = list("mean", "median")) {
  ic <- ee$ImageCollection(ic_url)
  tdy_ic <- as_tidyee(ic)
  tdy_ic_filt <- tdy_ic %>%
    filter(
      month %in% moi
    ) %>%
    group_by(year, month) %>%
    summarise(
      stat = stat
    )
  return(tdy_ic_filt)
}
