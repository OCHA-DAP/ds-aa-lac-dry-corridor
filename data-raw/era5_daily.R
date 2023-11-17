library(rgee)
library(tidyverse)
library(sf)
library(targets)
library(tidyrgee)
ee_Initialize()
fp_out <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public",
  "processed",
  "lac",
  "era5_raw_monthly.csv"
)
# Daily raw has data past 2020 and goes back to 1950
# daily only has 1974-2020
era <- ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_RAW")
date_df<- era %>% ee_get_date_ic() 
date_df$time_start %>% range()



era_filt <- era$filterDate("1981-01-01","2023-11-01")$select("total_precipitation_sum")

scale_val <- 27830 
era_tic <- as_tidyee(era_filt)

era_tic_monthly <- era_tic %>% 
  group_by(
    year,month
  ) %>% 
    summarise(
      stat= "sum"
    )
    

cat("get adm0 boundaries from FAO\n")
fc_adm0 = ee$FeatureCollection('FAO/GAUL/2015/level0')

fc_region_adm0 <- fc_adm0$filter(ee$Filter$inList("ADM0_NAME",  c("Guatemala",
                                                                  "Nicaragua",
                                                                  "Honduras",
                                                                  "El Salvador"))
)

cat("begin extraction of zonal stats\n")
df_rainfall_adm0 <- ee_extract_tidy(x = era_tic_monthly,
                                    y = fc_region_adm0,
                                    scale = scale_val,
                                    stat = "mean",
                                    via = "drive")


cat("write zonal sats to csv\n")
write_csv(x = df_rainfall_adm0,
          file = fp_out
)
cat("finished")