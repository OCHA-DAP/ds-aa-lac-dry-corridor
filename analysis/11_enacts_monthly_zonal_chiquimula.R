# 11_enacts_monthly_zonal_chiquimula.R
# Generate monthly zonal precipitation statistics from ENACTS for Chiquimula AOI
# Output saved to blob storage as parquet

box::use(
  cumulus,
  sf,
  dplyr[...]
)

box::use(
  ../R/enacts
)

# Load Chiquimula AOI from fieldmaps
gdf_adm1 <- cumulus$download_fieldmaps_sf(iso = "gtm", layer = "gtm_adm1")$gtm_adm1

gdf_chiquimula <- gdf_adm1 |>
  filter(ADM1_ES == "Chiquimula")

# Load ENACTS precipitation raster
r_enacts <- enacts$load_precip_enacts(wrap = FALSE)

# Calculate monthly zonal statistics
df_monthly <- enacts$monthly_zonal(
  r = r_enacts,
  zone = gdf_chiquimula
)

# Add AOI metadata
df_monthly <- df_monthly |>
  mutate(
    iso3 = "GTM",
    pcode = "GT20",
    adm1_name = "Chiquimula"
  )

# Save to blob storage
cumulus$blob_write(
  df = df_monthly,
  container = "projects",
  name = "ds-aa-lac-dry-corridor/data/processed/enacts/enacts_monthly_zonal_chiquimula.parquet"
)

cat("Saved to blob: ds-aa-lac-dry-corridor/data/processed/enacts/enacts_monthly_zonal_chiquimula.parquet\n")
