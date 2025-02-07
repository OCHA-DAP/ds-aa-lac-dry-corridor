box::use(
  dplyr,
  ggplot2,
  readr,
  stringr,
  tidyr,
  cumulus,
  lubridate,
  logger
)

box::use(
  utils = ../utils/gen_utils,
  # blob = ../email/email_utils/blob_utils,
  ../datasources/insivumeh
)
box::reload(utils)




# get_run_date <- function(run_date = Sys.Date()){
#   return(R)
# 
# }
# 
# if
# run_date <- Sys.Date()
run_date <- lubridate$as_date("2024-06-01") #DELETE LATER OR INCORPORATE TEST FUNC
activation_moment_month <-  lubridate$floor_date(run_date, "month")

insiv_received <- insivumeh$insivumeh_availability(run_date = activation_moment_month)

# this threshold table dictates which thresholds and forecast source we use.
df_thresholds <- utils$load_threshold_table(file_name="df_thresholds_seas5_insivumeh_adm1_refined.parquet")
df_thresholds |> 
  dplyr$arrange(
    iso3
  )


df_relevant_thresholds <- df_thresholds |> 
  filter(
    issued_month_label == month(activation_moment_month, abbr= T, label =T)
  )


# this function can/should be edited to reflect changes in monitoring AOI
df_aoi <- utils$load_aoi_df()

df_admin_name_lookup <- cumulus$blob_load_admin_lookup()

box::reload(utils)
gdf_adm1 <- utils$load_adm1_sf()


gdf_aoi_gtm <- gdf_adm1 |> 
  dplyr$filter(
    ADM1_PCODE == df_aoi[df_aoi$iso3 == "GTM",]$pcode
  ) |> 
  summarise()

gdf_adm1_cadc <- cumulus$blob_read(
  container = "projects",
  name =  "ds-aa-lac-dry-corridor/framework_update_2025/gdf_cadc_adm1.parquet",
  as_data_frame =FALSE
) |> 
  st_as_sf()


logger$log_info("Getting lastest SEAS5 forecast from Postgres")
box::reload(insivumeh)

load_available_forecasts <- function(df = df_relevant_thresholds){
  if("INSIVUMEH" %in% df$forecast_source){
    logger$log_info("Loading INSIVUMEH Raster")
    r_insiv <- insivumeh$load_ncdf_blob_insiv(run_date = activation_moment_month)
    logger$log_info("Running INSIVUMEH Zonal Stats")
    names(r_insiv)
    dfz_insiv <- insivumeh$zonal_insivumeh(r =r_insiv, zone = gdf_aoi_gtm)
    cumulus$seas5_aggregate_forecast(dfz_insiv, value = "value",valid_months = 5:8,     by = c("adm0_es","issued_date"))
    dfz_insiv_postrera <- cumulus$seas5_aggregate_forecast(dfz_insiv, value = "value",valid_months = 9:11,     by = c("adm0_es","issued_date"))
  }

  }

  insiv_available <- utils$insivumeh_availability()
  if(insiv_available & month(run_date)!=5){
    insivumeh$load_ncdf_blob_insiv(run_date = activation_moment_month)
  }
}
box::reload(insivumeh)
r_insiv <- insivumeh$load_ncdf_blob_insiv(run_date = activation_moment_month)

con <- cumulus$pg_con()
df_seas5 <- tbl(con,"seas5") |> 
  filter(
    adm_level == 1,
    pcode %in% df_aoi$pcode,
    issued_date ==activation_moment_month
  ) |> 
  collect()

df_seas5_mm <- df_seas5 |> 
  dplyr$mutate(
    mean_mm = lubridate$days_in_month(forecast_issue_date)*mean
  )

df_seas5 <- cumulus$seas5_aggregate_forecast(
  df_seas5_mm,
  value = "mean_mm",
  valid_months = c(5:8),
  by = c("iso3","pcode","issued_date")
) |> 
  dplyr$mutate(
    forecast_source = "SEAS5"
  )


