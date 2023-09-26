# Download monthly historical CHIRPS precipitation from GEE
# you will need a GEE account and `{rgee}` set up (https://github.com/r-spatial/rgee)

# script meant to be run as a background process through terminal
# i.e on mac I do `caffeinate -s Rscript data-raw/chirps_adm0_monthly.R`
# You may want to run `ee_Initialize()` out of background processing if you face an authentication error


library(rgee)
library(tidyverse)
library(tidyrgee)
library(targets)
ee_Initialize()
tar_load(gdf_aoi_adm)

out_dir <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public",
  "processed",
  "lac"
) 


cat("reading and manipulate image collection\n")
ic <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")
tic <- as_tidyee(ic)

tic_monthly_rainfall <- tic %>% 
  group_by(year,month) %>% 
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
df_rainfall_adm0 <- ee_extract_tidy(x = tic_monthly_rainfall,
                y = fc_region_adm0,
                scale = 5566,
                stat = "mean",
                via = "drive")


cat("write zonal sats to csv\n")
write_csv(x = df_rainfall_adm0,
          file = file.path(
            out_dir,
            "chirps_monthly_adm0.csv"
          )
)
cat("finished")





