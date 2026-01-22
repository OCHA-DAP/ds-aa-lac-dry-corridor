#' code to make potential new threshodls for 2025 monitoring based on a
#' new refined AOI of selected admin 1 levels.

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

forecast_model_yyyymm <- "202506"


# Prep Meta Lookup --------------------------------------------------------

df_aoi <- utils$load_aoi_df()

df_lookup <- cumulus$blob_load_admin_lookup() |>
  clean_names()


con <- cumulus$pg_con()

tbl_polys <- tbl(con,"polygon") |>
  filter(adm_level == 1)

df_polys <- tbl_polys |>
  filter(
    iso3 %in% c("NIC","HND","SLV","GTM")
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


# Insivumeh ---------------------------------------------------------------

AOI_GTM <- df_new_aoi_meta |> 
  filter(iso3=="GTM") |> 
  pull(name)



dir_insivumeh <- file.path(
  Sys.getenv("AA_DATA_DIR_NEW"),
  "private",
  "raw",
  "lac",
  "INSIVUMEH"
)

dir_update <-  file.path(
  dir_insivumeh,
  "202506_format"
)


r_update <- insivumeh$load_ncdf_insivumeh(
  gdb = dir_update,wrap=F,
  file_rgx_pattern = "^pronos_deterministic_forecast_"
  )


gdf_adm1 <- cumulus$download_fieldmaps_sf(iso = "gtm",layer = "gtm_adm1")$gtm_adm1

# if more admins added to Guatemala we need to dissolve them before running 
# zonal stats.
gdf_gtm_aoi <- gdf_adm1 |> 
  filter(
    ADM1_ES %in% AOI_GTM
  ) |> 
  group_by( 
    ADM0_ES
  ) |> 
  summarise(
    do_union = TRUE
  )



dfz <- exact_extract(
  x = r_update,
  y = gdf_gtm_aoi,
  fun = "mean"
)|> 
  pivot_longer(everything()) %>%
  separate(name, into = c("stat", "issued_date", "lt_chr"), sep = "\\.") %>%
  mutate(
    adm0_es = "Guatemala",
    issued_date = as_date(issued_date),
    leadtime = parse_number(lt_chr),
    valid_date = issued_date + months(leadtime),
    adm1_es = glue_collapse(AOI_GTM,sep =",")
  )

dfz$issued_date |> range()




# `cumulus$seas5_aggregate_forecast()` basically should work perfectly 
# for any seasonal monthly forecast.. perhaps i should change the name in
# the package
df_primera_insiv <- cumulus$seas5_aggregate_forecast(
  dfz,
  value ="value",
  valid_months = c(5:8),
  by = c("adm0_es","issued_date")
)


df_postrera_insiv <- cumulus$seas5_aggregate_forecast(
  dfz,
  value ="value",
  valid_months = c(9:11),
  by = c("adm0_es","issued_date")
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
          by = c("adm0_es","leadtime"),
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
        group_by(adm0_es,issued_month_label = month(issued_date,abbr=T,label = T),leadtime) |> 
        summarise(
          calc_empirical_rp_level = list(approxfun(rp_emp, value,method = "linear", rule =2,yright =Inf)),
          calc_empirical_rp = list(approxfun( value,rp_emp,method = "linear",rule =2))
        )
    }
  )


ldf_rp4_by_lt <- rp_linear_funcs |> 
  map(\(dft){
    dft |> 
      group_by(adm0_es, leadtime,issued_month_label) |> 
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
    AOI = AOI_SET_NAME,
    iso3 = "GTM"
  ) 


# quick check
df_primera_insiv$issued_date |> range()
df_postrera_insiv |> 
  filter(
    issued_date == as_date("2025-07-01")|
    issued_date == as_date("2025-06-01")
  )
  

df_thresholds_old <- cumulus$blob_read(
  name = "ds-aa-lac-dry-corridor/framework_update_2025/df_thresholds_seas5_insivumeh_adm1_refined.parquet"
)

df_thresholds_old_no_gtm <- df_thresholds_old |> 
  filter(iso3 != "GTM")

df_combined_thresholds <- bind_rows(
  df_thresholds_old_no_gtm,
  df_insiv_thresholds
)



containers<- cumulus$blob_containers(stage = "dev",write_access = TRUE)
if(OVERWRITE){
  cumulus$blob_write(
    df = df_combined_thresholds,
    container = containers$projects,
    stage = "dev",
    name = glue(
      "ds-aa-lac-dry-corridor/framework_update_2025/df_thresholds_seas5_insivumeh_adm1_refined_insiv_update_{forecast_model_yyyymm}.parquet"
    )
  )  
}
