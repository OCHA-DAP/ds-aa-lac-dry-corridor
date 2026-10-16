#' Lightweight utilities for 2026 monitoring
#'
#' Contains only the functions needed by the 2026 monitoring script,
#' without the heavy transitive deps (RNetCDF, terra, etc.) that
#' gen_utils.R pulls in via insivumeh.

box::use(
  dplyr[...],
  rlang[...],
  tibble[...],
  cumulus,
  sf,
  geoarrow[...],
  janitor
)


#' @export
load_adm1_sf <- function() {
  cumulus$blob_read(
    container = "projects",
    name = "ds-aa-lac-dry-corridor/framework_update_2025/gdf_cadc_adm1.parquet",
    as_data_frame = FALSE
  ) |>
    sf$st_as_sf() |>
    janitor$clean_names()
}


#' @export
load_aoi_df <- function(version = c("2026", "2026_startnetwork")) {
  version <- arg_match(version)

  if (version == "2026") {
    ret <- tribble(
      ~pcode, ~iso3,               ~name,
      "GT20", "GTM",        "Chiquimula",
      "GT21", "GTM",            "Jalapa",
      "GT02", "GTM",      "El Progreso",
      "GT19", "GTM",            "Zacapa",
      "HN07", "HND",        "El Paraiso",
      "HN08", "HND", "Francisco Morazan",
    )
    # Note: SLV uses adm_level=0 (country-level) and is handled separately
  }
  if (version == "2026_startnetwork") {
    ret <- tribble(
      ~pcode, ~iso3,               ~name,
      "GT14", "GTM",           "Quiche",
      "GT15", "GTM",     "Baja Verapaz",
    )
  }
  ret
}
