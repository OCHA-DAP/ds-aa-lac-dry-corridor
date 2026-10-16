# RQ7 — Where IS the growing season? Agricultural-drought-risk mask from JRC ASAP.
#
# Our drought analysis flags dryness everywhere; agricultural drought risk only matters where
# there is (a) meaningful cropland and (b) an ACTIVE growing season during our drought windows.
# ASAP gives both, purpose-built: asap_mask_crop_v03 (cropland fraction, 0-200) and
# inseason_month{1-12} (per-pixel, is that month in the active season). We compute, per GAUL
# admin-1: cropland fraction, and the fraction of that cropland in active season during primera
# (May-Aug) and postrera (Sep-Nov). This is a better crop mask than MODIS and adds SEASON TIMING.

suppressMessages({ library(terra); library(sf); library(exactextractr); library(dplyr) })

AA <- Sys.getenv("AA_DATA_DIR")
RD <- file.path(AA, "public/raw/glb/asap/reference_data")
SEAS <- file.path(AA, "public/processed/glb/asap/season/month")
out_csv <- "data-raw/mission_support_2026/rq7_growing_season"

gaul <- st_read("data-raw/mission_support_2026/_shared/gaul_adm1_gtm_hnd_slv.geojson", quiet = TRUE) |>
  janitor::clean_names() |>
  st_make_valid() |>
  st_collection_extract("POLYGON") |>
  st_cast("MULTIPOLYGON")
ca <- ext(-93, -82.5, 12.3, 18.7)   # crop the global rasters to Central America first

crop_mask <- crop(rast(file.path(RD, "asap_mask_crop_v03.tif")), ca)   # 0-200 (=% cropland x2)
inseason <- rast(file.path(SEAS, sprintf("inseason_month%d.tif", 5:11))) |> crop(ca)
crs(inseason) <- crs(crop_mask)     # inseason rasters ship without a CRS; same lon/lat grid
inseason[inseason > 1] <- NA        # 0/1 valid; 251-255 = nodata

# a pixel is "primera-active" if in season in ANY of May-Aug (layers 1:4), postrera = Sep-Nov (5:7)
primera_active <- app(inseason[[1:4]], fun = function(x) as.numeric(any(x == 1, na.rm = TRUE)))
postrera_active <- app(inseason[[5:7]], fun = function(x) as.numeric(any(x == 1, na.rm = TRUE)))
primera_active[is.na(primera_active)] <- 0
postrera_active[is.na(postrera_active)] <- 0
# align to the crop-mask grid, then cropland-in-season = crop_mask where in-season
primera_active  <- resample(primera_active, crop_mask, method = "near")
postrera_active <- resample(postrera_active, crop_mask, method = "near")

# Per admin-1: cropland fraction, and share of CROPLAND that is in-season each window
# (sum of crop pixels in-season / sum of all crop pixels).
crop_sum <- exact_extract(crop_mask, gaul, "sum")
gaul$crop_frac         <- exact_extract(crop_mask / 200, gaul, "mean")
gaul$primera_inseason  <- exact_extract(crop_mask * primera_active, gaul, "sum") / crop_sum
gaul$postrera_inseason <- exact_extract(crop_mask * postrera_active, gaul, "sum") / crop_sum

res <- st_drop_geometry(gaul) |>
  transmute(gaul_code = adm1_code, adm0_name, adm1_name,
            crop_frac, primera_inseason, postrera_inseason) |>
  mutate(iso3 = recode(adm0_name, Guatemala = "GTM", Honduras = "HND", `El Salvador` = "SLV"))
readr::write_csv(res, file.path(out_csv, "growing_season_by_adm1.csv"))

cat("== Cropland fraction + growing-season overlap, by admin-1 (top cropland units) ==\n")
res |> arrange(desc(crop_frac)) |>
  transmute(iso3, adm1_name, crop_frac = round(crop_frac, 3),
            primera = round(primera_inseason, 2), postrera = round(postrera_inseason, 2)) |>
  head(15) |> as.data.frame() |> print()

cat("\n== Country summary (cropland-weighted) ==\n")
res |> group_by(iso3) |>
  summarise(mean_crop_frac = round(mean(crop_frac, na.rm = TRUE), 3),
            primera_active = round(weighted.mean(primera_inseason, crop_frac, na.rm = TRUE), 2),
            postrera_active = round(weighted.mean(postrera_inseason, crop_frac, na.rm = TRUE), 2),
            .groups = "drop") |> as.data.frame() |> print()

cat("\nUnits with negligible cropland (crop_frac < 0.05):",
    sum(res$crop_frac < 0.05, na.rm = TRUE), "of", nrow(res), "\n")
