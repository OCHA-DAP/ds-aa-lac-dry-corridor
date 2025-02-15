#' requirements for 2025 monitoring.
#' 
#' 
required_pkgs <- c(
  "RNetCDF", "terra", "sf", "dplyr", "exactextractr", "lubridate", "ggplot2", 
  "ggrepel", "tidyr", "janitor", "stringr", "glue", "rlang", "purrr", "forcats", 
  "readr", "gt", "arrow", "geoarrow", "AzureStor", "ncmeta", "logger", "blastula", 
  "tibble", "showtext", "remotes", "box", "dbplyr", "RPostgres", "curl"
)


install.packages(required_pkgs)

# Ensure remotes package is installed before using it
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("OCHA-DAP/cumulus")

# Additional packages
install.packages("gghdx")

# # List of required packages
# required_pkgs <- c(
#   "RNetCDF",
#   "terra",
#   "sf", 
#   "dplyr",
#   "exactextractr",
#   "lubridate",
#   "ggplot2", 
#   "ggrepel",
#   "tidyr",
#   "janitor",
#   "stringr",
#   "glue",
#   "rlang", 
#   "purrr",
#   "forcats",
#   "readr",
#   "gt", 
#   "arrow",
#   "geoarrow",
#   "AzureStor", 
#   "ncmeta",
#   "logger",
#   "blastula",
#   "tibble",
#   "showtext",
#   "remotes",
#   "box",
#   "dbplyr",
#   "RPostgres",
#   "curl"
# )
# install.packages(required_pkgs)
# 
# install.packages(c(
#   "gghdx"
# )
# )
# 
# remotes::install_github(
#   repo = "OCHA-DAP/cumulus"
# )
# install.packages("gghdx")
# # once we have `{remotes}` should be able to install_github


