# Pull GAUL admin geometries (adm1 + adm2) for GTM/HND/SLV once, save as GeoJSON.
# These carry ADM1_CODE/ADM2_CODE that match BOTH the CHIRPS pull output AND FAO ASI adm1_code
# -> lets us plot/join the FAO-family products by GAUL code with zero name-matching (robust at
# adm2 where CODAB name joins collide). CODAB/pcode stays for the DB products (ERA5/IMERG/SEAS5).

suppressMessages({ library(rgee); library(sf); library(dplyr) })
ee_Initialize()
out_dir <- "data-raw/mission_support_2026/_shared"
COUNTRIES <- c("Guatemala", "Honduras", "El Salvador")

pull_geom <- function(asset, tag) {
  fc <- ee$FeatureCollection(asset)$
    filter(ee$Filter$inList("ADM0_NAME", ee$List(as.list(COUNTRIES))))
  sf_obj <- ee_as_sf(fc, via = "getInfo") |> janitor::clean_names()
  out <- file.path(out_dir, paste0("gaul_", tag, "_gtm_hnd_slv.geojson"))
  sf::st_write(sf_obj, out, delete_dsn = TRUE, quiet = TRUE)
  message("wrote ", out, " (", nrow(sf_obj), " features)")
  sf_obj
}

g1 <- pull_geom("FAO/GAUL_SIMPLIFIED_500m/2015/level1", "adm1")
g2 <- pull_geom("FAO/GAUL_SIMPLIFIED_500m/2015/level2", "adm2")
message("DONE.")
