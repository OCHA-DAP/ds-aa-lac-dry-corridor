#' wrote classified historical data to ds-aa-cerf-global-trigger-allocations
#' directory on blob for use in financial exposure analysis repo.

box::use(AzureStor)
box::use(arrow)

box::use(../src/email/email_utils/email_utils[load_drive_file])
box::use(../src/email/email_utils/blob_utils[load_proj_contatiners])
box::use(targets[...])
box::use(dplyr[...])

bc <- load_proj_contatiners()

df_cds_insivumeh_thresholds_rp4 <-  load_drive_file(
  dribble = drive_dribble,
  file_name = "thresholds_CDs_INSIV.rds"
)

# all thresholds based on rp of 4 for each leadtime

# For Guatemala we use thresholds based on analysis of INSIVUMEH data at all 
# activation moments EXCEPT leadtime 0 for the primera season. This is because
# Guatemala does not produce a leadtime 0 like ECMWF does. Therefore
# we use ECMWF for that one activation moment: May  (primera season)
df_framework_thresholds <- df_cds_insivumeh_thresholds_rp4 |> 
  filter(
    !(adm0_es=="Guatemala" & window == "primera" & forecast_source == "ECMWF CDs" & lt %in% c(1,2,3) ),
    !(adm0_es=="Guatemala" & window == "postera" & forecast_source == "ECMWF CDs")
  )

tar_load(df_cds_seasonal_summarised) # SEAS 5 CD historical data
tar_load(df_insuvimeh_seasonal_summarised_filtered2) # Insivumeh historical data

df_insuvimeh_seasonal_summarised_filtered2 |> 
  count(adm0_es, window, lt)

# need to filter the historical data in the same way to use ECMWF for everything
# Except Guatemala where INSIVUMEH is given preference for all activation moments
# that it is available
df_cds_filt <- df_cds_seasonal_summarised |> 
  mutate(
    forecast_source = "ECMWF CDs"
  ) |> 
  filter(
    !(adm0_es == "Guatemala" &
    window == "primera" & 
    lt %in% c(1,2,3)),
  !(
    adm0_es == "Guatemala" & window =="postera"
  )
  ) 

df_historical <- bind_rows(
  df_cds_filt,
  df_insuvimeh_seasonal_summarised_filtered2 |> 
    mutate(
      forecast_source = "INSIVUMEH"
    )
    ) 

df_historical_classified <- df_historical |> 
  left_join(
    df_framework_thresholds,
    by = c("adm0_es", "window", "forecast_source", "lt")
  ) |> 
  mutate(
    flag = mm < q_val
  )

arrow$write_parquet(
  x = df_historical_classified,
  sink = tf <- tempfile(fileext = ".parquet")
)

AzureStor$upload_blob(
  container = bc$PROJECTS_CONT,
  src = tf, 
  dest = "ds-aa-cerf-global-trigger-allocations/df_cadc_historical_classified.parquet"
  
)

  