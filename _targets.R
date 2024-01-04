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

fp_iri_prob <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "processed",
  "lac",
  "iri",
  "lac_iri_forecast_seasonal_precipitation_tercile_prob_Np18Sp10Em83Wm93.nc"
)
gdb_ecmwf_mars_tifs <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "processed",
  "lac",
  "ecmwf_seasonal",
  "seas51",
  "mars"
)



list(
  tar_target(
    name = gdf_aoi_adm,
    command = read_rds(
      file.path(
        Sys.getenv("AA_DATA_DIR"),
        "public",
        "processed",
        "lac",
        "lac_all_cod_adms.rds"
      )
    )
  ),
  tar_target(
    name = df_iri_adm0,
    command = iri_nc_to_r(
      tnc_object = tidync(fp_iri_prob),
      type = "prob"
    ) %>%
      imap_dfr(
        \(rtmp, nm){
          # loop through each leadtime - grab prob bavg and run zonal stats
          # scary things happen if you don't deepcopy
          r_bavg <- deepcopy(rtmp[[1]]) # bavg
          exact_extract(
            x = r_bavg,
            y = gdf_aoi_adm$adm0,
            fun = c("mean", "median"),
            append_cols = c("adm0_es", "adm0_pcode")
          ) %>%
            pivot_longer(-matches("adm0_")) %>%
            separate(name, into = c("stat", "date"), sep = "\\.") %>%
            pivot_wider(names_from = "stat", values_from = "value") %>%
            # add season information by combing dates w/ leadtimes
            mutate(
              leadtime = parse_number(nm),
              predict_start_mo = as_date(date) + months(leadtime),
              predict_mo1_abbr = str_sub(month(predict_start_mo, abbr = T, label = T), start = 1, end = 1),
              predict_mo2_abbr = str_sub(month(predict_start_mo + months(1), abbr = T, label = T), start = 1, end = 1),
              predict_mo3_abbr = str_sub(month(predict_start_mo + months(2), abbr = T, label = T), start = 1, end = 1),
              end_yr_abbr = format(predict_start_mo + months(2), "%y")
            ) %>%
            unite(
              "seas", predict_mo1_abbr:end_yr_abbr,
              sep = ""
            )
        }
      )
  ),
  tar_target(
    name = df_aoi_pixel_values,
    command = iri_nc_to_r(
      tnc_object = tidync(fp_iri_prob),
      type = "prob"
    ) %>%
      imap_dfr(\(rt, nm){
        # [[1]] first element is bavg (c1)
        rt_copy <- deepcopy(rt[[1]])

        # clip to aoi (instead of just bbox)
        rt_clipped <- mask(
          rt_copy,
          # dissolve adm0 to 1 poly
          gdf_aoi_adm$adm0 %>%
            summarise()
        )
        rt_clipped %>%
          terra::values() %>%
          data.frame() %>%
          pivot_longer(everything()) %>%
          mutate(
            leadtime = str_replace(nm, "lt", "leadtime "),
            prob_cat = case_when(
              value < 40 ~ "lt 40",
              value < 50 ~ "lt 50",
              value >= 50 ~ "gte 50"
            )
          )
      })
  ),
  tar_target(
    name = df_cropland_lte_vhi_threshold,
    command = cropland_lte_vhi_thresh(

      # all monthly VHI raster directory
      vhi_raster_dir = file.path(
        Sys.getenv("AA_DATA_DIR"),
        "private",
        "raw",
        "lac",
        "vhi_fao"
      ),
      # file path to cropland fraction raster
      cropland_raster_fp = file.path(
        Sys.getenv("AA_DATA_DIR"),
        "public",
        "raw",
        "glb",
        "cropland",
        "GlcShare_v10_02",
        "glc_shv10_02.Tif"
      ),

      # admin 0 polygon
      poly = gdf_aoi_adm$adm0,

      # retain these cols in zonal stats
      poly_cols = c(
        "adm0_es",
        "adm0_pcode"
      ),
      simplify_poly = 0.01,

      # All VHI thresholds to run
      threshold_seq = seq(0.05, 1, by = 0.05)
    )
  ),
  tar_target(
    name = r_ecmwf_mars,
    command = load_mars_raster(gdb = gdb_ecmwf_mars_tifs)
  ),
  tar_target(
    name = df_ecmwf_mars,
    command = zonal_ecmwf_mars(r_wrapped = r_ecmwf_mars, zone = gdf_aoi_adm$adm0, stat = "mean")
  )
)
