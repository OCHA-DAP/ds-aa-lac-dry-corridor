# Extract zonal statistics for Chiquimula (GT20)
# This script extracts INSIVUMEH forecast data from local NetCDF files
# ENACTS and SEAS5 data are now loaded via R/enacts.R and R/seas5.R modules
# Run this once (or when source data updates), then use outputs in comparison scripts

library(tidyverse)
library(terra)
library(sf)
library(exactextractr)
library(RNetCDF)
library(ncmeta)
library(lubridate)
library(cumulus)
library(janitor)

# =============================================================================
# CONFIGURATION
# =============================================================================

AOI_NAME <- "chiquimula"
AOI_PCODE <- "GT20"

PRIMERA_MONTHS <- 5:8
POSTRERA_MONTHS <- 9:11

# Output paths - blob storage
BLOB_OUTPUT <- paste0(
  "ds-aa-lac-dry-corridor/data/processed/insivumeh_special/",
  "insivumeh_special_models_zonal_seasonal_", AOI_NAME, ".parquet"
)

# =============================================================================
# LOAD AOI
# =============================================================================

cat("=== Loading AOI:", AOI_NAME, "(", AOI_PCODE, ") ===\n")

sf_gtm_adm1 <- cumulus::download_fieldmaps_sf(iso3 = "GTM", layer = "gtm_adm1")$gtm_adm1
aoi <- sf_gtm_adm1 |> clean_names()|> filter(adm1_pcode == AOI_PCODE)
cat("AOI loaded:", nrow(aoi), "polygon(s)\n")

# =============================================================================
# INSIVUMEH LOADING FUNCTIONS
# =============================================================================


# similar functions as before -- but data structure has changed a little bit
# so just keeping them self contained here.

r_extent <- function(nc_ob) {
  lat <- var.get.nc(nc_ob, "Y")
  lon <- var.get.nc(nc_ob, "X")
  dx <- diff(lon[1:2])
  dy <- abs(diff(lat[1:2]))
  c(min(lon) - dx/2, max(lon) + dx/2, min(lat) - dy/2, max(lat) + dy/2)
}

load_insivumeh_nc <- function(fp) {
  valid_time_meta <- nc_att(fp, "T", "units")$value[[1]]
  start_time_meta <- nc_att(fp, "S", "units")$value[[1]]

  valid_date <- as_date(str_extract(valid_time_meta, "\\d{4}-\\d{2}-\\d{2}"))
  pub_date <- as_date(str_extract(start_time_meta, "\\d{4}-\\d{2}-\\d{2}"))
  lt <- interval(pub_date, valid_date) %/% months(1)
  model <- str_extract(fp, "(CCSM4|CESM1|CFSv2)")

  bname <- paste0(pub_date, ".lt_", lt, ".", model)

  dat <- open.nc(fp)
  dat_extent <- r_extent(dat)
  value_array <- var.get.nc(dat, "deterministic")
  value_array_fixed <- aperm(value_array, c(2, 1))
  close.nc(dat)

  r <- rast(x = value_array_fixed, ext = dat_extent, crs = "OGC:CRS84")
  names(r) <- bname
  r
}

extract_nc_metadata <- function(fp) {
  valid_time_meta <- nc_att(fp, "T", "units")$value[[1]]
  start_time_meta <- nc_att(fp, "S", "units")$value[[1]]

  valid_date <- as_date(str_extract(valid_time_meta, "\\d{4}-\\d{2}-\\d{2}"))
  pub_date <- as_date(str_extract(start_time_meta, "\\d{4}-\\d{2}-\\d{2}"))
  lt <- interval(pub_date, valid_date) %/% months(1)
  model <- str_extract(fp, "(CCSM4|CESM1|CFSv2)")

  tibble(
    filepath = fp,
    model = model,
    pub_date = pub_date,
    valid_date = valid_date,
    lt = lt,
    bname = paste0(pub_date, ".lt_", lt, ".", model)
  )
}

load_insivumeh_stack <- function(filepaths) {
  lr <- map(filepaths, \(fp) {
    cat(".")
    load_insivumeh_nc(fp)
  })
  cat("\n")
  rast(lr)
}

zonal_insivumeh <- function(r, zones, zone_id_col) {
  df_zonal <- exact_extract(x = r, y = zones, fun = "mean", append_cols = zone_id_col)

  df_zonal |>
    pivot_longer(cols = -all_of(zone_id_col), names_to = "name", values_to = "value") |>
    rename(aoi = all_of(zone_id_col)) |>
    separate(name, into = c("stat", "pub_date", "lt_chr", "model"), sep = "\\.") |>
    mutate(
      pub_date = as_date(pub_date),
      lt = parse_number(lt_chr),
      valid_date = pub_date + months(lt)
    ) |>
    select(aoi, pub_date, valid_date, lt, model, value)
}

aggregate_forecast_seasonal <- function(df, valid_months, by) {
  df |>
    rename(issued_date = pub_date, leadtime = lt) |>
    # from cumulus
    seas5_aggregate_forecast(
      value = "value",
      valid_months = valid_months,
      by = by
    )
}

# =============================================================================
# EXTRACT INSIVUMEH
# =============================================================================

cat("\n=== Extracting INSIVUMEH for", AOI_NAME, "===\n")

# it's alot faster to load a raster stack from locally synced data
# therefore, for now it's on GDRIVE - will also eventually copy to blob
forecast_dir <- file.path(
  Sys.getenv("AA_DATA_DIR_NEW"),
  "private/raw/lac/INSIVUMEH/special_forecast_historical_20260112",
  "pronosticos historicos"
)

all_files <- list.files(forecast_dir, pattern = "\\.nc$", recursive = TRUE, full.names = TRUE)
prcp_files <- all_files[str_detect(all_files, "_PRCP/")]

cat("Found", length(prcp_files), "precipitation files\n")
cat("Extracting metadata...\n")
df_meta <- map(prcp_files, extract_nc_metadata, .progress = TRUE) |> list_rbind()
df_unique <- df_meta |> distinct(bname, .keep_all = TRUE)
cat("Unique forecasts after deduplication:", nrow(df_unique), "\n")

insivumeh_models <- c("CFSv2", "CCSM4", "CESM1")

process_insivumeh_model <- function(model_name, df_unique, zones, zone_id_col) {
  cat("Processing", model_name, "...\n")

  model_files <- df_unique |> filter(model == model_name) |> pull(filepath)
  cat("  Loading", length(model_files), "files...")
  r_stack <- load_insivumeh_stack(model_files)

  cat("  Extracting zonal stats...\n")
  df_zonal <- zonal_insivumeh(r_stack, zones, zone_id_col)

  df_primera <- df_zonal |>
    aggregate_forecast_seasonal(PRIMERA_MONTHS, by = c("aoi", "issued_date", "model")) |>
    mutate(window = "primera", year = year(issued_date))

  df_postrera <- df_zonal |>
    aggregate_forecast_seasonal(POSTRERA_MONTHS, by = c("aoi", "issued_date", "model")) |>
    mutate(window = "postrera", year = year(issued_date))

  bind_rows(df_primera, df_postrera) |>
    mutate(forecast_source = paste0("INSIVUMEH_", model_name))
}

df_insiv <- map(insivumeh_models, \(m) {
  process_insivumeh_model(m, df_unique, aoi, "adm1_es")
}) |>
  list_rbind() |>
  mutate(aoi_pcode = AOI_PCODE, aoi_name = AOI_NAME)

cat("INSIVUMEH extraction complete:", nrow(df_insiv), "rows\n")
cumulus::blob_write(
  df = df_insiv,
  container = "projects",
  name = BLOB_OUTPUT
)
cat("Saved to blob:", BLOB_OUTPUT, "\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=== EXTRACTION COMPLETE ===\n")
cat("AOI:", AOI_NAME, "(", AOI_PCODE, ")\n")
cat("Output (blob):", BLOB_OUTPUT, "\n")
cat("Year range:", min(df_insiv$year), "-", max(df_insiv$year), "\n")
cat("\nNote: ENACTS and SEAS5 data now loaded via R/enacts.R and R/seas5.R modules\n")
