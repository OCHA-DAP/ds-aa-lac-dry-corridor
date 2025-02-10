#' requirements for 2025 monitoring.

# List of required packages
required_pkgs <- c(
  "RNetCDF",
  "terra",
  "sf", 
  "dplyr",
  "exactextractr",
  "lubridate",
  "ggplot2", 
  "ggrepel",
  "tidyr",
  "janitor",
  "stringr",
  "glue",
  "rlang", 
  "purrr",
  "gghdx",
  "forcats",
  "readr",
  "gt", 
  "arrow",
  "geoarrow",
  "AzureStor", 
  "ncmeta",
  "logger",
  "blastula",
  "tibble",
  "showtext",
  "remotes",
  "box"
)
install.packages(required_pkgs)

# once we have `{remotes}` should be able to install_github
remotes::install_github(
  repo = "OCHA-DAP/cumulus"
  )

