# Description -------------------------------------------------------------
#' **Title**  Central American Dry Corridor (CADC) Trigger Inputs
#' **Overview** Simplified pipeline to generate thresholds using forecast data available from
#'  1. ECMWF MARS SEAS51 (since 1981)
#'  2. INSUVIMEH  (since 1981)
#' Exploratory analysis will be added back in a modular fashion. Removed from this version to facilitate review
#' **Details**
#' The analysis is performed over the 4 countries through which the CADC runs (Nicaragua, Guatemala, Honduras, El Salvador).



# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # dynanic file branching?
tar_source()

# packages that your targets need to run
tar_option_set(
  packages = c(
    "tidyverse",
    "exactextractr",
    "terra",
    "sf",
    "RNetCDF"
  )
)

options(clustermq.scheduler = "multicore")

# Define simple inputs -----------------------------------------------------------

gdb_ecmwf_mars_tifs <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "processed",
  "lac",
  "ecmwf_seasonal",
  "seas51",
  "mars"
)

gdb_ecmwf_cds_tifs <- file.path(Sys.getenv("AA_DATA_DIR"),
                            "private",
                            "processed",
                            "lac",
                            "ecmwf_seasonal",
                            "seas51",
                            "tif"
)


gdb_insuvimeh_gtm <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "raw",
  "lac",
  "INSUVIMEH",
  "Pronósticos_Precip_NextGen_Guatemala"
)

list(
  ## Load admin boundaries ####
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
    name = gdf_aoi_adm0_no_islands,
    command = read_rds(
      file.path(
        Sys.getenv("AA_DATA_DIR"),
        "public",
        "processed",
        "lac",
        "lac_cadc_adm0_no_islands.rds"
      )
    )
  ),
  # ECMWF MARS --------------------------------------------------------------
  
  tar_target(
    description = "MARS Catalogue - PackedSpatRaster object containing each publication month leadtime combination as and individual band.",
    name = r_ecmwf_mars,
    command = load_ecmwf_mars_stack(gdb = gdb_ecmwf_mars_tifs,
                                      rm_from_ly_name="lac_seasonal-montly-individual-members_tprate-",
                                      wrap=T),
  ),
  tar_target(
    description = "MARS -data.frame in long format containing zonal means for each publication month-leadtime combination",
    name = df_ecmwf_mars,
    command = zonal_ecmwf(r_wrapped = r_ecmwf_mars, zone = gdf_aoi_adm$adm0, stat = "mean")
  ),
  tar_target(
    description = "MARS -data.frame where monthly rainfall values have been summed to seasons of interest for trigger (MJJA,SON)",
    name = df_mars_seasonal_summarised,
    command = summarise_seasons(
      df = df_ecmwf_mars %>%
        rename(mm = "value"), # get in same format as other
      window_list = list(
        "primera" = c(5, 6, 7, 8),
        "postera" = c(9, 10, 11)
      )
    )
  ),
  tar_target(
    description = "MARS - data.frame (long format) where quantiles defined by RPs 1-10 re provided for each season, country, leadtime",
    name = df_mars_q_summary,
    command = grouped_quantile_summary(
      df = df_mars_seasonal_summarised,
      x = "mm",
      rps = c(1:10),
      grp_vars = c("adm0_es", "window", "lt")
    )
  ),
  
  # ECMWF CDS ---------------------------------------------------------------
  tar_target(
    description = "Copernicus Data Store (CDs)- PackedSpatRaster object containing each publication month leadtime combination as and individual band.",
    name = r_ecmwf_cds,
    command = load_ecmwf_cd_stack(gdb = gdb_ecmwf_cds_tifs,wrap = T),
  ),
  tar_target(
    description = "CDs - data.frame in long format containing zonal means for each publication month-leadtime combination",
    name = df_ecmwf_cds,
    command = zonal_ecmwf(r_wrapped = r_ecmwf_cds, 
                               zone = gdf_aoi_adm0_no_islands,
                               stat = "mean") %>% 
      mutate(
        # raster still in average mm/hour
        value = lubridate::days_in_month(valid_date)*24*3600*1000 * value
      )
  ),
  
  tar_target(
    description = "CDs data.frame where monthly rainfall values have been summed to seasons of interest for trigger (MJJA,SON)",
    name = df_cds_seasonal_summarised,
    command = summarise_seasons(
      df = df_ecmwf_cds %>%
        rename(mm = "value"), # get in same format as other
      window_list = list(
        "primera" = c(5, 6, 7, 8),
        "postera" = c(9, 10, 11)
      )
    )
  ),
  tar_target(
    description = "CDs - data.frame (long format) where quantiles defined by RPs 1-10 re provided for each season, country, leadtime",
    name = df_cds_q_summary,
    command = grouped_quantile_summary(
      df = df_cds_seasonal_summarised %>% 
        # standardize to MARs date range
        filter(year(pub_date)<2023),
      x = "mm",
      rps = c(1:10),
      grp_vars = c("adm0_es", "window", "lt")
    )
  ),
  # INSUVIMEH
  tar_target(
    description = "Catalogue of all data received from INSUVIMEH. Useful for troubleshooting at earlier stages when gaps.Gaps have been filled, but still might be useful in future",
    name = df_gtm_nextgen_catalogue,
    command = catalogue_insuvimeh_files(
      gdb = gdb_insuvimeh_gtm,
      file_name_pattern = "\\d{4}.nc$"
    ),
  ),
  
  # load raster
  tar_target(
    description = "PackedSpatRaster object containing INSUVIMEH forecast with each publication month leadtime combination as and individual band.",
    name = r_wrap_gtm_nextgen,
    command = load_insuvimeh_raster(gdb = gdb_insuvimeh_gtm)
  ),
  tar_target(
    description = "PackedSpatRaster object containing INSIVIMEH forecast with each publication month leadtime combination as and individual band.",
    name = r_wrap_gtm_nextgen2,
    command = load_insuvimeh_raster2(gdb = gdb_insuvimeh_gtm)
  ),
  
  # run zonal stats on insuvimeh
  tar_target(
    description = "INSUVIMEH monthly zonal means by leadtime",
    name = df_gtm_nextgen_adm0,
    command = zonal_gtm_insuvimeh(
      r = unwrap(r_wrap_gtm_nextgen),
      gdf = gdf_aoi_adm,
      rm_dup_years = F
    )
  ),
  # summarise by window, leadtime & pub date
  tar_target(
    description = "INSUVIMEH seasonal forecast sums by leadtime/window",
    name = df_insuvimeh_seasonal_summarised,
    command = summarise_seasons(
      df = df_gtm_nextgen_adm0 %>%
        rename(mm = "value"), # get in same format as other
      window_list = list(
        "primera" = c(5, 6, 7, 8),
        "postera" = c(9, 10, 11)
      )
    )
  ),
  
  # final decision is to use ECMWF for final month of monitoring to
  # include May forecast in May publication and to not include Sep monitoring of Postrera
  tar_target(
    description = "INSUVIMEH seasonal summaries with May removed from Primera monitoring",
    name = df_insuvimeh_seasonal_summarised_filtered,
    command = df_insuvimeh_seasonal_summarised %>%
      # used through 2022 for ECMWF - so let's keep it standardized also let's not make threshold
      # harder to hit because of drought that is still largely in effect (2023)
      filter(year(pub_date) < 2023)
  ),
  
  # these will be used for to threshold Guatemala.
  tar_target(
    description = "INSUVIMEH historical record broken into quantile classes by window/lt",
    name = df_insuvimeh_q_summary,
    command = grouped_quantile_summary(
      df = df_insuvimeh_seasonal_summarised_filtered,
      x = "mm",
      rps = c(1:10),
      grp_vars = c("adm0_es", "window", "lt")
    )
  ),
  tar_target(
    description = "INSUVIMEH monthly zonal means by leadtime",
    name = df_gtm_nextgen2_adm0,
    command = zonal_gtm_insuvimeh(
      r = unwrap(r_wrap_gtm_nextgen2),
      gdf = gdf_aoi_adm,
      rm_dup_years = F
    )
  ),
  # summarise by window, leadtime & pub date
  tar_target(
    description = "INSUVIMEH seasonal forecast sums by leadtime/window",
    name = df_insuvimeh_seasonal_summarised2,
    command = summarise_seasons(
      df = df_gtm_nextgen2_adm0 %>%
        rename(mm = "value"), # get in same format as other
      window_list = list(
        "primera" = c(5, 6, 7, 8),
        "postera" = c(9, 10, 11)
      )
    )
  ),
  
  # final decision is to use ECMWF for final month of monitoring to
  # include May forecast in May publication and to not include Sep monitoring of Postrera
  tar_target(
    description = "INSUVIMEH seasonal summaries with May removed from Primera monitoring",
    name = df_insuvimeh_seasonal_summarised_filtered2,
    command = df_insuvimeh_seasonal_summarised2 %>%
      # used through 2022 for ECMWF - so let's keep it standardized also let's not make threshold
      # harder to hit because of drought that is still largely in effect (2023)
      filter(year(pub_date) < 2023)
  ),
  
  # these will be used for to threshold Guatemala.
  tar_target(
    description = "INSUVIMEH historical record broken into quantile classes by window/lt",
    name = df_insuvimeh_q_summary2,
    command = grouped_quantile_summary(
      df = df_insuvimeh_seasonal_summarised_filtered2,
      x = "mm",
      rps = c(1:10),
      grp_vars = c("adm0_es", "window", "lt")
    )
  ),
  
  # final decision was to go w/ RP 4 so saving this for easy manipulation in
  # analysis/threshold_tables.qmd
  tar_target(
    description = "ECMWF & INSUVIMEH Thresholds in long format",
    name = df_all_thresholds_rp4,
    command = bind_rows(
      df_insuvimeh_q_summary %>%
        filter(rp == 4) %>%
        mutate(
          forecast_source = "INSIVUMEH"
        ),
      df_mars_q_summary %>%
        filter(rp == 4) %>%
        mutate(
          forecast_source = "ECMWF MARS"
        )
    )
  ),
  tar_target(
    description = "CDS ECMWF & INSUVIMEH Thresholds in long format",
    name = df_cds_insivumeh_thresholds_rp4,
    command = bind_rows(
      df_insuvimeh_q_summary %>%
        filter(rp == 4) %>%
        mutate(
          forecast_source = "INSUVIMEH"
        ),
      df_cds_q_summary %>%
        filter(rp == 4) %>%
        mutate(
          forecast_source = "ECMWF CDs"
        )
    )
  )
  # will add exploratory modules here.
)
