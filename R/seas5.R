box::use(
  dplyr[...],
  purrr[map, list_rbind],
  lubridate[year]
)

#' load_seas5_seasonal
#' @description
#' Load SEAS5 seasonal forecast data from database, aggregated to Primera and
#' Postrera seasons for Guatemala dry corridor.
#'
#' @param iso3 character, ISO3 country code (default "gtm")
#' @param admin_level integer, admin level (default 1)
#' @param adm_name character, admin unit name (default "Chiquimula")
#' @param seasons named list of month vectors (default primera = 5:8, postrera = 9:11)
#' @return data.frame with columns: issued_date, leadtime, value, window, year,
#'         forecast_source, aoi_pcode, aoi_name
#' @export
#' @examples \dontrun{
#' df_seas5 <- load_seas5_seasonal()
#' df_seas5 <- load_seas5_seasonal(adm_name = "Zacapa")
#' }
load_seas5_seasonal <- function(
    iso3 = "gtm",
    admin_level = 1,
    adm_name = "Chiquimula",
    seasons = list(primera = 5:8, postrera = 9:11)
) {
  # Load raw monthly forecast data from database
  df_raw <- cumulus::pg_load_seas5_historical(
    iso3 = iso3,
    adm_level = admin_level,
    adm_name = adm_name
  )

  # Get pcode for metadata
  aoi_pcode <- unique(df_raw$pcode)

  # Aggregate to seasonal totals
  df_seasonal <- names(seasons) |>
    map(\(window_name) {
      cumulus::seas5_aggregate_forecast(
        df_raw,
        value = "mean",
        valid_months = seasons[[window_name]],
        by = c("pcode", "issued_date")
      ) |>
        rename(value = mean) |>
        mutate(
          window = window_name,
          year = year(issued_date)
        )
    }) |>
    list_rbind()

  # Add metadata columns to match expected format
  df_seasonal |>
    mutate(
      forecast_source = "SEAS5",
      aoi_pcode = aoi_pcode,
      aoi_name = tolower(adm_name)
    )
}
