#' cropland_lte_vhi_thresh
#' @description
#' General: Calculate % cropland (per country) ≤ to a sequence of VHI thresholds
#' Given a directory containing  monthly historical VHI rasters and file path to FAO cropland fraction raster
#' Iterate through a sequence of VHI threshold and determin the % area of cropland less than or equal to each threshold
#' @param vhi_raster_dir \code{character} directory containing monthly historical VHI rasters
#' @param cropland_raster_fp \code{character} file path containing FAO cropland fraction raster file
#' @param poly sf polygon area of interest (aoi)
#' @param poly_cols \code{character} columns in polygon to return in resulting data.frame with zonal stats
#' @param simplify_poly dtolerance spec to simplify polygon for faster zonal stats (default =0.01)
#' @param threshold_seq sequence of thresholds
#'
#' @return data.frame containing specified 'poly_cols' and % cropland for each record that is covered by VHI values ≤ specified threshold

cropland_lte_vhi_thresh <- function(
    vhi_raster_dir = file.path(
      Sys.getenv("AA_DATA_DIR"),
      "private",
      "raw",
      "lac",
      "vhi_fao"
    ),
    cropland_raster_fp = file.path(
      Sys.getenv("AA_DATA_DIR"),
      "public",
      "raw",
      "glb",
      "cropland",
      "GlcShare_v10_02",
      "glc_shv10_02.Tif"
    ),
    poly = gdf_aoi_adm$adm0,
    poly_cols = c(
      "adm0_es",
      "adm0_pcode"
    ),
    simplify_poly = 0.01,
    threshold_seq = seq(0.05, 1, by = 0.05)) {
  
  fp_r_vhi <- list.files(vhi_raster_dir, full.names = T)
  chr_yyyymm <- str_extract(basename(fp_r_vhi), "\\d{4}-\\d{2}")
  r_vhi <- rast(fp_r_vhi)
  r_vhi %>%
    set.names(chr_yyyymm)

  r_vhi_clean <- deepcopy(r_vhi)
  # 250-255 all quality flags - make NA
  r_vhi_clean[r_vhi_clean >= 250] <- NA

  # read in crop data
  r_cropland <- rast(cropland_raster_fp)


  # set CRS - double check this
  terra::crs(r_cropland) <- terra::crs(r_vhi)

  # to combine cropland and vhi raster sets they will need same extents resolution
  # crop
  cat("resampling cropland to VHI raster set \n")
  r_cropland_bounded <- crop(r_cropland, y = r_vhi)

  # set same extent
  ext(r_cropland_bounded) <- ext(r_vhi)

  # resample cropland to match -- go from 1km -> 30m ?
  r_cropland_resampled <- resample(r_cropland_bounded, y = r_vhi)

  # simplify poly
  poly_main <- poly %>%
    st_cast("POLYGON") %>%
    mutate(
      area = as.numeric(st_area(.))
    ) %>%
    filter(
      area > 1e10
    )

  poly_simp <- poly_main %>%
    group_by(!!!syms(poly_cols)) %>%
    summarise() %>%
    st_simplify(dTolerance = simplify_poly)



  # quickly sum of all crop fraction pixels per country - use this later to calculate %
  df_total_cropland <- exact_extract(
    x = r_cropland_resampled,
    y = poly_simp,
    fun = "sum",
    append_cols = poly_cols
  ) %>%
    ungroup() %>%
    rename(sum_crop_pixels = "sum")


  df_sum_cropland_lte <- threshold_seq %>%
    map_dfr(\(thresh){
      cat(thresh,"\n")

      
      # not sure why this doesn't work at all!
      
      # m <- c(
      #   -Inf, thresh, 1,
      #   thresh, Inf, 0
      # )
      # 
      # reclass_matrix <- matrix(m, 
      #                          ncol = 3,
      #                          byrow = TRUE)
      # 
      # r_vhi_reclassed <- classify(
      #   r_vhi_clean,
      #   reclass_matrix, 
      #   include.lowest = T
      #   )

      r_vhi_binary <- deepcopy(r_vhi_clean)
      r_vhi_binary[r_vhi_binary>thresh] <- NA
      r_vhi_binary[!is.na(r_vhi_binary)]<-1
  

  
      cat("multiplying binary VHI again crop frac\n")
      r_cropland_lte_vhi_thresh <- r_vhi_binary * r_cropland_resampled

      cat("zonal stats - sum pixels ≤ ", thresh,"\n")
      
      exact_extract(
        x = r_cropland_lte_vhi_thresh,
        y = poly_simp,
        fun = "sum",
        append_cols = poly_cols
      ) %>%
        pivot_longer(-matches("adm")) %>%
        separate(name,
          into = c("stat", "date"),
          sep = "\\."
        ) %>%
        mutate(threshold = thresh)
    })
  
  ret <- df_sum_cropland_lte %>%
    rename(
      sum_crop_pixels_lte_thresh = "value"
    ) %>%
    select(-stat) %>%
    left_join(
      df_total_cropland,
      by = poly_cols
    ) %>%
    mutate(
      pct_crop_pixels_lte_thresh = sum_crop_pixels_lte_thresh / sum_crop_pixels
    )

  return(ret)
}
