# Description -------------------------------------------------------------
#' **Title**  Central American Dry Corridor (CADC) Exploratory Data Analysis (EDA) Pipeline
#' **Overview** Pipeline provides basic intermediate analysis objects derived primarily from the historical analysis of:
#'  1. IRI tercile probability (2017-current)
#'  2. ECMWF SEAS51 (1981-current)
#'  3. VHI FAO (1981-current)
#' **Details** 
#' The analysis is performed over the 4 countries through which the CADC runs (Nicaragua, Guatemala, Honduras, El Salvador).
#' 
#' The critical targets generated here are further described, and visualized in the following notebooks:
#'  1. 
#' The insights gained from this EDA here and subsequent notebooks are used as the basis to define the thresholds and realtime
#' monitoring framework found **[here](insert_link)**
#' **Note** The pipeline takes a few hours (47 min for cadc vhi extr) to run due to the VHI thresholding extractions.


# Load packages required to define the pipeline:
library(targets)
tar_source()

# packages that your targets need to run
tar_option_set(
  packages = c("tidyverse",
               "exactextractr",
               "terra",
               "tidync",
               "sf",
               "tidyrgee",
               "tidync",
               "rnaturalearth",
               "sf",
               "rgee",
               "janitor"
  ) 
)

options(clustermq.scheduler = "multicore")

# Define simple inputs -----------------------------------------------------------

## GDB - CADC delineation ####
gdb_cadc_border <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "raw",
  "lac",
  "CSC_limite"
  
)

## GDB - IRI ####
fp_iri_prob <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "processed",
  "lac",
  "iri",
  "lac_iri_forecast_seasonal_precipitation_tercile_prob_Np18Sp10Em83Wm93.nc"
)

## GDB - ECMWF ####
dir_ecmwf_tifs <- file.path(Sys.getenv("AA_DATA_DIR"),
                            "private",
                            "processed",
                            "lac",
                            "ecmwf_seasonal",
                            "seas51",
                            "tif"
)


list(
  ## Load admin boundaries ####
  tar_target(
    name = gdf_aoi_adm,
    command = load_proj_admins() %>%
      map(~ .x %>%
            st_make_valid() %>%
            select(matches("^adm\\d_[ep]")))
  ),
  ## Load CADC border ####
  tar_target(
    name = gdf_dc,
    command = st_read(gdb_cadc_border,"CSC_limite") %>%
      st_intersection(y = gdf_aoi_adm$adm0) %>%  
      group_by(adm0_es) %>% 
      summarise() %>% # dissolve
      st_make_valid()
  ),
  
  # IRI ---------------------------------------------------------------------
  
  ## IRI Zonal ####
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
          r_bavg <- deepcopy(rtmp[[1]]) # below avg
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
  
  ## IRI - Extract pixel values ####
  
  # this target used to visualize distribution of pixel values
  tar_target(
    name = df_aoi_pixel_values,
    command =iri_nc_to_r(
      tnc_object = tidync(fp_iri_prob),
      type = "prob"
    ) %>% 
      imap_dfr(\(rt,nm){
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
            leadtime = str_replace(nm,"lt", "leadtime "),
            prob_cat = case_when(
              value <40~ "lt 40",
              value <50 ~"lt 50",
              value >=50 ~"gte 50")
          )
      }
      )
  ),
  # ECMWF -------------------------------------------------------------------
  ## ECMWF Zonal Stats ####
  
  # get total precip each month and aggregate to country boundary by mean and median.
  tar_target(
    name = df_ecmwf_zonal,
    command= aggregate_ecmwf_historical (dir_ecmwf = dir_ecmwf_tifs,
                                         init_trimester_month=c(5,6,9),
                                         zonal_boundary = gdf_aoi_adm$adm0)
  ),
  ## ECMWF Pixel Values ####
  # tar_target(
  #   name = df_ecmwf_pixel,
  #   command = aggregate_ecmwf_historical_pixel(dir_ecmwf = dir_ecmwf_tifs,
  #                                              init_trimester_month=c(5,6,9))
  # ),
  
  # VHI ---------------------------------------------------------------------
  
  ## % Cropland ≤ VHI Thresh ####
  
  # iterate through different thresholds on VHI and get % cropland below each threshold 
  # for all historical VHI data for each country.
  
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
  
  # same as above, but this time on the dry corridor boundary for each country.
  tar_target(
    name = df_cropland_lte_vhi_threshold_dc,
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
      poly = gdf_dc,
      
      # retain these cols in zonal stats
      poly_cols = c(
        "adm0_es"
      ),
      simplify_poly = 0.0,
      
      # All VHI thresholds to run
      threshold_seq = seq(0.05, 1, by = 0.05)
    )
  ),
  
  ## VHI - Zonal mean/median ####
  tar_target(
    name = df_vhi_median_over_crop,
    command = cropland_vhi_pt_estimate(
      
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
      simplify_poly = 0.01
    )
  )
)



