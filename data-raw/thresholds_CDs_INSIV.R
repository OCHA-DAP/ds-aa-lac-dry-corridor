#' script to archive threshold table as parquet file on blob.

library(targets)
library(dplyr)

tar_load(
  df_cds_insivumeh_thresholds_rp4,
  store=here('_targets')
)

cumulus::write_az_file(
    df_cds_insivumeh_thresholds_rp4,
    name = "ds-aa-lac-dry-corridor/thresholds_CDs_INSIV.parquet",
    container = "projects"
)

