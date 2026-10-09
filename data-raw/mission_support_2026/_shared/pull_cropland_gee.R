# Pull cropland fraction per admin unit (GAUL adm1 + adm2) via GEE — a static crop mask/weight
# for the priority synthesis (rq5). rq1 concluded the rainfall hazard is broad, so targeting
# needs a crop-exposure layer: don't prioritise units with negligible agriculture.
#
# Source: MODIS MCD12Q1 IGBP land cover (500m). Cropland = class 12 (croplands) + 14
# (cropland/natural-vegetation mosaic). cropland_frac = mean(landcover in {12,14}) per unit.
# Joins to ASI/CHIRPS by GAUL code (ADM1_CODE/ADM2_CODE).

suppressMessages({ library(rgee); library(dplyr); library(readr) })
ee_Initialize()
out_dir <- "data-raw/mission_support_2026/_shared"
COUNTRIES <- c("Guatemala", "Honduras", "El Salvador")

lc <- ee$ImageCollection("MODIS/061/MCD12Q1")$
  filter(ee$Filter$date("2022-01-01", "2023-01-01"))$first()$select("LC_Type1")
cropland <- lc$eq(12)$Or(lc$eq(14))$rename("cropland_frac")

pull_crop <- function(asset, code_field, tag) {
  fc <- ee$FeatureCollection(asset)$
    filter(ee$Filter$inList("ADM0_NAME", ee$List(as.list(COUNTRIES))))
  res <- ee_extract(x = cropland, y = fc, fun = ee$Reducer$mean(), scale = 500,
                    via = "getInfo", sf = FALSE)
  out <- file.path(out_dir, paste0("cropland_frac_", tag, "_gtm_hnd_slv.csv"))
  write_csv(res, out)
  message("wrote ", out, " (", nrow(res), " units)")
  res
}

pull_crop("FAO/GAUL_SIMPLIFIED_500m/2015/level1", "ADM1_CODE", "adm1")
pull_crop("FAO/GAUL_SIMPLIFIED_500m/2015/level2", "ADM2_CODE", "adm2")
message("DONE.")
