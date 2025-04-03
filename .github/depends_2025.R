#' requirements for 2025 monitoring.
#' 
#' 
required_pkgs <- c(
  "RNetCDF", "terra", "sf", "dplyr", "exactextractr", "lubridate", "ggplot2", 
  "ggrepel", "tidyr", "janitor", "stringr", "glue", "rlang", "purrr", "forcats", 
  "readr", "gt", "arrow", "geoarrow", "AzureStor", "ncmeta", "logger", 
  "tibble", "showtext", "remotes", "box", "dbplyr", "RPostgres", "curl"
)


install.packages(required_pkgs)

# Ensure remotes package is installed before using it
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("OCHA-DAP/cumulus")

# update to curl that seems to fix it when running locally
remotes::install_github("rstudio/blastula")

# Additional packages
install.packages("gghdx")




