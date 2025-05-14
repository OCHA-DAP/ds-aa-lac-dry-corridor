#' code to create intermediate objects/files for startNetwork analysis
#' outputs:
#'   1. `df_startnetwork_historical_zonal_primera.parquet` : historical zonal 
#'   statistics for 2 startNetwork provinces in Guatemala. This is only done for
#'   INSIVUMEH data because we can get the zonal stats for ECMWF directly 
#'   from postgres DB
#'   2. `df_thresholds_seas5_insivumeh_startnetwork_guatemela.parquet`:
#'   thresholds for 2 provinces per lead time from both INSIVUMEH and ECMWF

box::use(
  ../src/datasources/insivumeh,
  utils = ../ src/utils/gen_utils
  )

box::use(
  terra, 
  cumulus,
  dplyr[...],
  purrr[...],
  sf[...],
  exactextractr[...],
  tidyr[...],
  readr[...],
  lubridate[...],
  janitor[...],
  
  glue[...],
  rlang[...],
  ggplot2[...],
  gghdx[...],
  stringr[...],
  ggrepel[...]
  
)
gghdx()

LAST_BASELINE_YEAR <-  2022
AOI_SET_NAME <- "Selected admin 1's"
OVERWRITE <- FALSE
OVERWRITE_ZONAL_FILE <- FALSE


# Prep Meta Lookup --------------------------------------------------------

df_aoi <- utils$load_aoi_df(version = "startnetwork")

df_lookup <- cumulus$blob_load_admin_lookup() |>
  clean_names()

df_lookup |> 
  filter(
    adm_level ==1,
    adm0_name == "Guatemala",
    str_detect(adm1_name,"Baja Verapaz|Quich")
  )

con <- cumulus$pg_con()

tbl_polys <- tbl(con,"polygon") |>
  filter(adm_level == 1)

df_polys <- tbl_polys |>
  filter(
    iso3 %in% c("GTM")
  ) |>
  collect() |>
  clean_names()



df_poly_meta <- df_polys |>
  left_join(
    df_lookup |>
      select(iso3,matches("adm\\d"),matches("adm_level")) |>
      filter(adm_level ==1),
    by = c("iso3" = "iso3","pcode"="adm1_pcode","adm_level")
  )

df_new_aoi_meta <- df_poly_meta |>
  filter(
    pcode %in% df_aoi$pcode
  )



# Prep Seas5 --------------------------------------------------------------

df_seas5_adm1 <- tbl(con,"seas5") |> 
  filter(
    adm_level == 1,
    pcode %in% df_aoi$pcode,
    month(valid_date)%in% c(5:11) # just grabbing all relevant months for both windows
  ) |> 
  collect() # this actually loads data into memory so can take a 10s or so.

df_seas5 <- df_seas5_adm1 |> 
  mutate(
    precipitation = days_in_month(valid_date) * mean
  )


df_primera <- cumulus$seas5_aggregate_forecast(
  df_seas5,
  value = "precipitation",
  valid_months =c(5:8),
  by = c("iso3", "pcode","issued_date")
) 

df_postrera <- cumulus$seas5_aggregate_forecast(
  df_seas5,
  valid_months =c(9:11),
  value = "precipitation",
  by = c("iso3", "pcode","issued_date")
)




ldf_seas <- list(
  "primera"= df_primera,
  "postrera" = df_postrera
) |> 
  map(
    \(dft){
      dft |> 
        left_join(df_new_aoi_meta, by =c("iso3", "pcode"))
      
      # Here is the code we would normally use to do the weighted area-analysis
      # leaving here in case different decision taken
      
        # group_by(iso3, issued_date, leadtime, valid_month_label) |> 
        # summarise(
        #   mm = weighted.mean (precipitation, w =seas5_n_upsampled_pixels ),
        #   .groups="drop"
        # )
    }
  )


ldf_thresholded_1981_2022 <- ldf_seas |> 
  map(
    \(dft){
      dft |> 
        mutate(
          issued_month_label = month(issued_date,label = TRUE, abbr=T)
        ) |> 
        filter(
          year(issued_date)<=LAST_BASELINE_YEAR
        ) |> 
        # this func classifies each record RP and creates a boolean of whether or not the threshold
        # is passed. We ignore the boolean and just grab the rp_empiricaal values for interpolation
        utils$threshold_var(
          var= "precipitation",
          by = c("iso3","pcode","name","leadtime","issued_month_label"),
          rp_threshold = 4,
          direction =-1
        ) 
    }
  )

# now we can linearly interpolate:
rp_linear_funcs <- ldf_thresholded_1981_2022 |> 
  map(
    \(dft){
      dft |> 
        group_by(iso3,pcode, name,leadtime, issued_month_label) |> 
        summarise(
          calc_empirical_rp_level = list(approxfun(rp_emp, precipitation,method = "linear", rule =2,yright =Inf)),
          calc_empirical_rp = list(approxfun( precipitation,rp_emp,method = "linear",rule =2)),.groups="drop"
        )
    }
  )

# interpolate for RP 4
df_seas5_thresholds <- rp_linear_funcs |> 
  imap(\(dft,nmt){
    dft |> 
      group_by(iso3, pcode, name,leadtime, issued_month_label) |> 
      reframe(
        RP_empirical = 4,
        value_empirical = map_dbl(RP_empirical,calc_empirical_rp_level),
      ) |> 
      mutate(
        window = nmt
      )
  }
  ) |> 
  list_rbind() |> 
  mutate(
    forecast_source = "SEAS5",
    AOI = name,
    adm0_es = "Guatemala"
  )




# Insivumeh ---------------------------------------------------------------

AOI_GTM <- df_new_aoi_meta |> 
  filter(iso3=="GTM") |> 
  pull(name)


# originally insivumeh `.nc` files saved on gdrive in different formats ( they
# have different extents). Therefore we run zonal stats on these different
# formats independently . More recently we've moved 2025 files to blob, so 
# those are also loaded and run separately
# 
dir_insivumeh <- file.path(
  Sys.getenv("AA_DATA_DIR_NEW"),
  "private",
  "raw",
  "lac",
  "INSIVUMEH"
)

dir_old <-  file.path(
  dir_insivumeh,
  "old_format"
)

# new format has new extent.
dir_new <- file.path(
  dir_insivumeh,
  "new_format"
)

r_old <- insivumeh$load_ncdf_insivumeh(gdb = dir_old,wrap=F)
r_new <- insivumeh$load_ncdf_insivumeh(gdb = dir_new,wrap=F)


# if we specify activatio moments we can use pre-made insivumeh function
# to load the insivumeh forecast directly from blob.

# these get rounded down to month
active_moments_insiv_gte_2025 <- c("2025-03-20","2025-04-20") 
lr_2025 <- map(
  active_moments_insiv_gte_2025,
  \(x){
    insivumeh$load_ncdf_blob_insiv(run_date= x)  
  }
)
r_2025 <- terra$rast(lr_2025)  
  
gdf_adm1 <- cumulus$download_fieldmaps_sf(iso = "gtm",layer = "gtm_adm1")$gtm_adm1

# startNetwork does not want regions merge - so we will not do the union as
# as done for other countries
gdf_gtm_aoi <- gdf_adm1 |> 
  filter(
    ADM1_ES %in% AOI_GTM
  ) 

lr <- list(
  "old" = r_old,
  "new" = r_new,
  "2025" = r_2025
)

# since new format has new extent, we just run the zonal statistics separtely
ldf <- map(lr,\(x){
  dft <- exact_extract(
    x = x,
    y = gdf_gtm_aoi,
    fun = "mean",
    append_cols = c("ADM1_ES","ADM1_PCODE")
  )
  dft |> 
    pivot_longer(-c("ADM1_ES","ADM1_PCODE")) |> 
    separate(name, into = c("stat", "issued_date", "lt_chr"), sep = "\\.") %>%
    mutate(
      adm0_es = "Guatemala",
      issued_date = as_date(issued_date),
      leadtime = parse_number(lt_chr),
      valid_date = issued_date + months(leadtime)
    )
  
})


dfz <- list_rbind(ldf) |> 
  clean_names()


# `cumulus$seas5_aggregate_forecast()` basically should work perfectly 
# for any seasonal monthly forecast.. perhaps i should change the name in
# the package
df_primera_insiv <- cumulus$seas5_aggregate_forecast(
  dfz,
  value ="value",
  valid_months = c(5:8),
  by = c("adm0_es","adm1_es","adm1_pcode","issued_date")
)

if(OVERWRITE_ZONAL_FILE){
  cumulus::blob_write(
    df = df_primera_insiv,
    container = "projects",
    name = "ds-aa-lac-dry-corridor/df_startnetwork_historical_zonal_primera.parquet"
  )  
}


df_postrera_insiv <- cumulus$seas5_aggregate_forecast(
  dfz,
  value ="value",
  valid_months = c(9:11),
  by = c("adm0_es","adm1_es","adm1_pcode","issued_date")
)

ldf_insiv <- list(
  "primera"= df_primera_insiv,
  "postrera"= df_postrera_insiv
)


ldf_thresholded_1981_2022 <- ldf_insiv |> 
  map(
    \(dft){
      dft |> 
        filter(
          year(issued_date)<=LAST_BASELINE_YEAR
        ) |> 
        utils$threshold_var(
          var= "value",
          by = c("adm0_es","adm0_es","adm1_es","leadtime"),
          rp_threshold = 4,
          direction =-1
        ) 
    }
  )

# now we can linearly interpolate:
rp_linear_funcs <- ldf_thresholded_1981_2022 |> 
  map(
    \(dft){
      dft |> 
        group_by(adm0_es,adm1_es, adm1_pcode,issued_month_label = month(issued_date,abbr=T,label = T),leadtime) |> 
        summarise(
          calc_empirical_rp_level = list(approxfun(rp_emp, value,method = "linear", rule =2,yright =Inf)),
          calc_empirical_rp = list(approxfun( value,rp_emp,method = "linear",rule =2))
        )
    }
  )


ldf_rp4_by_lt <- rp_linear_funcs |> 
  map(\(dft){
    dft |> 
      group_by(adm0_es, adm1_es, adm1_pcode,leadtime,issued_month_label) |> 
      reframe(
        RP_empirical = 4,
        value_empirical = map_dbl(RP_empirical,calc_empirical_rp_level),
      )
  }
  )

df_insiv_thresholds <- ldf_rp4_by_lt |> 
  imap(\(x,nmt){
    x |> 
      mutate(
        window = nmt
      )
  }
  ) |> 
  list_rbind() |> 
  mutate(
    forecast_source = "INSIVUMEH",
    AOI = adm1_es,
    iso3 = "GTM"
  ) 

df_combined_thresholds <- bind_rows(
  df_seas5_thresholds,
  df_insiv_thresholds
)

if(OVERWRITE){
  cumulus$blob_write(
    df = df_combined_thresholds,
    container = "projects",
    name = "ds-aa-lac-dry-corridor/framework_update_2025/df_thresholds_seas5_insivumeh_startnetwork_guatemela.parquet"
  )  
}
