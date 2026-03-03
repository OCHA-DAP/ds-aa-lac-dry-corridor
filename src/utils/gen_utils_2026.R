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


#' Load email recipients from blob CSV
#'
#' Moved from email_utils.R to avoid transitive blastula dependency.
#'
#' @param email_list character one of "core_developer", "developers",
#'   "internal_chd", "full_list", etc.
#' @return data.frame or list of data.frames (for full_list)
#' @export
load_email_recipients <- function(email_list) {
  df <- cumulus$blob_read(
    name = "ds-aa-lac-dry-corridor/framework_update_2025/202507update_email_recepients_cadc_trigger_2025.csv",
    container = "projects",
    stage = "dev"
  ) |>
    janitor$clean_names()

  ret <- df |>
    select(
      all_of(c("name", "organization", "role", "email", "email_group", "remove", email_list))
    ) |>
    filter(!is.na(!!sym(email_list)), remove != 1)

  if (email_list == "full_list") {
    split_raw <- split(ret, ret$email_group)
    ret <- list(
      group_a = bind_rows(split_raw$A, split_raw$Both),
      group_b = bind_rows(split_raw$B, split_raw$Both)
    )
  }
  ret
}
