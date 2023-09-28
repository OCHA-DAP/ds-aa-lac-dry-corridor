# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(rnaturalearth)
library(sf)
library(tidyverse)
library(rgee)
library(janitor)
library(rgee)
library(tidyrgee)
library(exactextractr)
library(terra)
library(tidync)

# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble") # packages that your targets need to run
)
options(clustermq.scheduler = "multicore")

tar_source()




list(
  tar_target(
    name = gdf_aoi_adm,
    command = load_proj_admins() %>%
      map(~ .x %>%
        st_make_valid() %>%
        select(matches("^adm\\d_[ep]")))
  )
)
