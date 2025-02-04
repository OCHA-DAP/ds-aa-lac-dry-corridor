#' wrote classified historical data to ds-aa-cerf-global-trigger-allocations
#' directory on blob for use in financial exposure analysis repo.

box::use(AzureStor)
box::use(arrow)
box::use(targets[...])
box::use(dplyr[...])
box::use(lubridate[...])
box::use(googledrive[drive_auth, drive_ls])
box::use(stringr[str_replace])


box::use(../src/email/email_utils/email_utils[load_drive_file])
box::use(../src/email/email_utils/blob_utils[load_proj_contatiners])




bc <- load_proj_contatiners()

# authenticate w/ service account
drive_auth(
  path=Sys.getenv("CADC_MONITORING_JSON")
)

drive_dribble <- drive_ls(
  corpus= "user"
)

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
  ) |> 
  mutate(
    forecast_source = str_replace(forecast_source, "INSUVIMEH","INSIVUMEH")
  )


tar_load(df_cds_seasonal_summarised) # SEAS 5 CD historical data
tar_load(df_insuvimeh_seasonal_summarised_filtered2) # Insivumeh historical data
tar_load(df_insuvimeh_seasonal_summarised_filtered) # Insivumeh historical data

df_insuvimeh_seasonal_summarised_filtered |> 
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
  ),
  # not monitorng in Feb for Primera,
  # not monitoring May or Sep for Postera
  !(month(pub_date)==2 & window=="primera"),
  !(month(pub_date)==5 & window=="postera"),
  !(month(pub_date)==9 & window=="postera")
  ) 

df_historical <- bind_rows(
  df_cds_filt,
  df_insuvimeh_seasonal_summarised_filtered |> 
    filter(
      !(month(pub_date)==2 & window=="primera"),
      !(month(pub_date)==5 & window=="postera"),
      # just adding here for clarity, but already Sep is already filtered
      # INSUVIMEH only gives pub_mo + 1, no  pub_mo = valid_mo forecast
      !(month(pub_date)==9 & window=="postera") 
    ) |> 
    mutate(
      forecast_source = "INSIVUMEH"
    )
    ) 

# df_historical_classified |>
#   group_by(
#    forecast_source
#   ) |> 
#   summarise(
#     min_date = min(pub_date),
#     max_date = max(pub_date)
#   )



df_historical_classified <- df_historical |> 
  arrange(adm0_es, pub_date) |> 
  left_join(
    df_framework_thresholds,
    by = c("adm0_es", "lt", "window", "forecast_source")
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

  