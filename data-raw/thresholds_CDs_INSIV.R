#' script to archive threshold table as parquet file on blob.

library(targets)
library(dplyr)

tar_load(
  df_cds_insivumeh_thresholds_rp4,
  store='_targets'
)

cumulus::write_az_file(
    df_cds_insivumeh_thresholds_rp4,
    name = "ds-aa-lac-dry-corridor/thresholds_CDs_INSIV.parquet",
    container = "projects"
)


library(googledrive)
library(readr)
write_rds(
  x= df_cds_insivumeh_thresholds_rp4, 
  file = tf <- tempfile(fileext = ".rds")
)

# authorize drive access
drive_auth(
  path = Sys.getenv("CADC_MONITORING_JSON")
)

drive_dribble <- drive_ls(
  corpus = "user"
)

folder_id<- drive_dribble %>% 
  filter(
    name == "CADC_GHA"
  ) %>% 
  pull(id)


thresholds_CDs_INSIV.parquet

drive_upload(
  media = tf,
  name = "thresholds_CDs_INSIV.rds",
  path = as_id(folder_id)
)


