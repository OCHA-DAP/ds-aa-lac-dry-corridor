# RQ7 script 02 — Precipitation-Sensitive Period (PSP) overlap with our drought windows.
#
# Sharper than "in-season": ASAP PSP marks the months when RAIN DEFICIT most damages the crop.
# Source: ds-eo-rasters PSP extraction (temp/psp_admin1_mask.tif) — 48-band 0.05deg boolean
# raster, bands {crop|rangeland}_s{1|2}_m{01..12}. Agricultural drought risk is highest where our
# drought window (primera May-Aug / postrera Sep-Nov) overlaps the crop PSP.

suppressMessages({ library(terra); library(sf); library(exactextractr); library(dplyr) })

AA  <- Sys.getenv("AA_DATA_DIR")
PSP <- "../ds-eo-rasters/temp/psp_admin1_mask.tif"   # sibling repo, local
out_csv <- "data-raw/mission_support_2026/rq7_growing_season"

gaul <- st_read("data-raw/mission_support_2026/_shared/gaul_adm1_gtm_hnd_slv.geojson", quiet = TRUE) |>
  janitor::clean_names() |> st_make_valid() |>
  st_collection_extract("POLYGON") |> st_cast("MULTIPOLYGON")
ca <- ext(-93, -82.5, 12.3, 18.7)

psp <- crop(rast(PSP), ca)
crop_mask <- crop(rast(file.path(AA, "public/raw/glb/asap/reference_data/asap_mask_crop_v03.tif")), ca)

# crop PSP: is a pixel precip-sensitive in ANY crop season during the window's months?
psp_window <- function(months) {
  bands <- as.vector(outer(c("crop_s1", "crop_s2"), sprintf("m%02d", months), paste, sep = "_"))
  bands <- intersect(bands, names(psp))
  app(psp[[bands]], fun = function(x) as.numeric(any(x == 1, na.rm = TRUE)))
}
primera_psp  <- psp_window(5:8)
postrera_psp <- psp_window(9:11)

# weight by cropland: resample crop mask to the PSP 0.05deg grid, sum-ratio
cm05 <- resample(crop_mask, primera_psp, method = "average")
crop_sum <- exact_extract(cm05, gaul, "sum")
gaul$primera_psp  <- exact_extract(cm05 * primera_psp, gaul, "sum") / crop_sum
gaul$postrera_psp <- exact_extract(cm05 * postrera_psp, gaul, "sum") / crop_sum

res <- st_drop_geometry(gaul) |>
  transmute(gaul_code = adm1_code, iso3 = recode(adm0_name, Guatemala = "GTM",
            Honduras = "HND", `El Salvador` = "SLV"), adm1_name,
            primera_psp = round(primera_psp, 2), postrera_psp = round(postrera_psp, 2))
readr::write_csv(res, file.path(out_csv, "psp_overlap_by_adm1.csv"))

cat("== Crop precipitation-sensitive-period overlap with our drought windows, by adm1 ==\n")
res |> arrange(iso3, desc(primera_psp)) |> as.data.frame() |> print()
cat("\n== country means ==\n")
res |> group_by(iso3) |> summarise(primera_psp = round(mean(primera_psp, na.rm = TRUE), 2),
      postrera_psp = round(mean(postrera_psp, na.rm = TRUE), 2), .groups = "drop") |>
  as.data.frame() |> print()
