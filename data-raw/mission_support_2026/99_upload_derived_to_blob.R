# Push chapter-13 derived inputs to blob: projects / ds-aa-lac-dry-corridor/book/static/
# One command refreshes the hosted snapshot the book reads at render time.
# Needs DSCI_AZ_BLOB_DEV_SAS_WRITE. Run from repo root: Rscript data-raw/mission_support_2026/99_upload_derived_to_blob.R
suppressMessages({library(cumulus); library(readr); library(sf)})
src <- "analysis/2026_cadc_drought_v3/data/mission_support"
dst <- "ds-aa-lac-dry-corridor/book/static/"
csvs <- c("primera_observed_2026.csv", "asi_primera_2026.csv", "mixed_primera_2026.csv",
          "pct_normal_2026.csv", "primera_vs_postrera_2026.csv", "priority_synthesis_adm1.csv",
          "season_specific_risk_adm1.csv", "early_vs_final_asi.csv")
for (f in csvs) {
  blob_write(read_csv(file.path(src, f), show_col_types = FALSE),
             paste0(dst, f), stage = "dev", container = "projects", progress_show = FALSE)
  cat("uploaded", f, "\n")
}
g <- st_read(file.path(src, "gaul_adm1_gtm_hnd_slv.geojson"), quiet = TRUE)
blob_write(g, paste0(dst, "gaul_adm1_gtm_hnd_slv.geojson"), stage = "dev", container = "projects", progress_show = FALSE)
cat("uploaded gaul_adm1_gtm_hnd_slv.geojson\n")
