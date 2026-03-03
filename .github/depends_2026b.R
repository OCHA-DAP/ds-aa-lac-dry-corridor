#' Requirements for 2026 monitoring (lightweight).
#'
#' Uses posit package manager for binary installs.
#' Drops RNetCDF, terra, exactextractr, ncmeta (not needed for 2026 pipeline).
#' Uses httr2 + base64enc for listmonk transactional email (no blastula).

PPM <- "https://packagemanager.posit.co/cran/__linux__/noble/latest"

install.packages(
  c(
    "dplyr", "forcats", "glue", "purrr", "readr", "stringr", "tidyr",
    "rlang", "tibble", "lubridate", "logger", "janitor",
    "sf", "ggplot2", "gt", "showtext",
    "arrow", "geoarrow", "AzureStor",
    "box", "dbplyr", "RPostgres", "DBI", "curl",
    "httr2", "base64enc"
  ),
  repos = PPM
)

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = PPM)
}

remotes::install_github("OCHA-DAP/cumulus")

install.packages("gghdx")
