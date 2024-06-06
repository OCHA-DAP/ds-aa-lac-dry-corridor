#' Description
#' Script for monitoring CADC trigger.
#' It needs to be run manually as natl forecast pipeline is not predictable.
#' Unfortunately, no getting around this as we need to manually check this data everytime.
#' 
#' Some aspects, of script are being set for eventual migration to GH Action, but in other
#' components this approach was abandoned -- eventually could be refactored to GH Action if
#' data pipelines become stable.

# library(aws.s3)
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)
library(tidyr) # separate
library(ggplot2)
library(forcats)
library(readr)

library(sf)
library(googledrive)
library(exactextractr)
# library(fs)
library(gt)
library(glue)
library(gghdx)
library(blastula)
# library(here)
# library(targets) # should remove for GHA eventually
library(terra)
gghdx()
  
# Sys.setenv(RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64")

fp_email_util_funcs <- list.files(
  file.path("src","email","email_utils"),
  full.names = T
)

walk(fp_email_util_funcs,~source(.x))

# 1. Input parameters --------------------------------------------------------

# Analysis depends on run date & matches that to the correct forecast publication
run_date <-  Sys.Date()
run_date_chr <-  format(floor_date(run_date, "month"),"%Y%m")
run_mo <-  month(run_date)

# publication and valid months of interest - these are set in the framework and should not be changed
# nonetheless if we want to see for example the May forecast of Primera for experimentation we can adjust here

primera_params <- list(
  pub_months = c(3,4,5),
  valid_months = c(5,6,7,8),
  trigger_season = "primera"
)
postrera_params <- list(
  pub_months = c(6,7,8),
  valid_months = c(9,10,11),
  trigger_season = "postrera"
)
# 2. Load Files --------------------------------------------------------------
# Load necessary files here
# 2. Load Files --------------------------------------------------------------

# I started setting this up using just googledrive package to allow data to be passed over during 
# a GH Action, but it's currently a bit complicated by the uncertainty of INSIVUMEH data.
# This is why there is a mix of input sources/access methods ({googledrive}, googledrive file paths, AWS).
# Everything here works as long as you have you're googledrive synced, but if pipeline is ever made secure we can transfer 
# everything to Azure cloud

# authenticate w/ service account
drive_auth(
  path=Sys.getenv("CADC_MONITORING_JSON")
  )

drive_dribble <- drive_ls(
  corpus= "user"
)

df_email_receps <- load_drive_file(
  dribble= drive_dribble,
  file_name = "email_recepients_cadc_trigger.csv"
)

# this file is for mapping
gdf_aoi <- load_drive_file(
  dribble = drive_dribble,
  file_name = "lac_cadc_adm0_no_islands.rds"
)

# these are just for mapping
adm0_simp <-  load_drive_file(
  dribble = drive_dribble,
  file_name = "central_america_aoi_adm0_simplified.rds"
)
adm1_simp <-  load_drive_file(
  dribble = drive_dribble,
  file_name = "central_america_aoi_adm1_simplified.rds"
)
adm0_surrounding_simp <-  load_drive_file(
  dribble = drive_dribble,
  file_name = "surrounding_aoi_adm0_simplified.rds"
)
df_cds_insivumeh_thresholds_rp4 <-  load_drive_file(
  dribble = drive_dribble,
  file_name = "thresholds_CDs_INSIV.rds"
)


# file path where .nc files from INSIVUMEH are being saved
insiv_gdb <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "raw",
  "lac",
  "INSUVIMEH",
  "new_format")

# load shape files for zonal extractoin
# targets::tar_load(gdf_aoi_adm)


# replaced with this with file accessed through google drive
# load thresholds -- includes all framework thresholds
# tar_load(
#   df_cds_insivumeh_thresholds_rp4
#   # store=here('_targets')
# )


# 3. Get Relevant Thresholds ------------------------------------------------
# Get relevant thresholds for Primera and Postrera
df_threshold_primera <- get_relevant_threshold(
  df_cds_insivumeh_thresholds_rp4 %>% 
    mutate(
      forecast_source = str_replace(forecast_source,"INSUVIMEH","INSIVUMEH")
    ),
  run_date, primera_params
  )

df_threshold_postrera <- get_relevant_threshold(
  df_cds_insivumeh_thresholds_rp4 |> 
    mutate(
      window = str_replace(window,"postera", "postrera")
    ), 
  run_date, 
  postrera_params
  )

is_primera <- nrow(df_threshold_primera)>0
is_postrera <- nrow(df_threshold_postrera)>0


# 4. Process Latest ECMWF ---------------------------------------------------

filename_cds_forecast <- paste0("cds_ecmwf_seas51_",floor_date(run_date,"month"),"_aoi.tif")
r_cds <- load_drive_file(
  dribble =drive_dribble,
  file_name = filename_cds_forecast
)
df_ecmwf_monthly <- r_cds %>% 
  zonal_ecmwf(zonal = gdf_aoi)



# Process ECMWF data for Primera and Postrera
if (is_primera) {
  df_ecmwf_primera_activation_status <- process_monthly_forecast(
    df_threshold = df_threshold_primera,
    df_forecast_monthly =  df_ecmwf_monthly,
    season_params = primera_params,
    forecast_source = "ECMWF CDs"
  )
}
if (is_postrera) {
  df_ecmwf_postrera_activation_status <- process_monthly_forecast(
    df_threshold_postrera, 
    df_ecmwf_monthly,
    season_params = postrera_params,
    forecast_source = "ECMWF CDs"
    )
}

# have to get rid of this for quick GHA run because relies on local files
# insiv_received <- insivumeh_received(gdb_base = insiv_gdb,
#                                      run_date = run_date)
insiv_received <- F
# 5. INSIVUMEH DATA ----------------------------------------------------------
# Process INSIVUMEH data if available
if (insiv_received) {
  gdf_gtm <- gdf_aoi %>% 
    filter(adm0_es=="Guatemala")
    ## 5b. load & process insivumeh raster ####
  r_insiv <-  load_ncdf_insivumeh(
    gdb =build_insiv_path(gdb_base =insiv_gdb,run_date = run_date )
  )
  ## 5c. Zonal Stats on INSIVUMEH raster ####
  df_monthly_gtm <- zonal_insivumeh(r_insiv,zone =gdf_gtm )
  
  if (is_primera & run_mo != 5) {
    insiv_primera_status_update <- process_monthly_forecast(
      df_threshold=df_threshold_primera, 
      df_forecast_monthly = df_monthly_gtm, 
      season_params= primera_params,
      forecast_source="INSIVUMEH"
      )
    df_framework_primera_activation_status <- merge_forecast_status(
      df_ecmwf_status =df_ecmwf_primera_activation_status,
      df_insiv_status =insiv_primera_status_update 
      )
  }
  if (is_postrera) {
    insiv_postrera_status_update <- process_monthly_forecast(
      df_threshold=df_threshold_postrera, 
      df_forecast_monthly = df_monthly_gtm, 
      season_params= postrera_params,
      forecast_source="INSIVUMEH"
      )
    
    df_framework_postrera_activation_status <- merge_forecast_status(
      df_ecmwf_status =df_ecmwf_postrera_activation_status,
      df_insiv_status =insiv_postrera_status_update 
    )
  }
}

# 6. Generate email graphics -------------------------------------------------
# Generate email graphics and text
if(is_primera){  
  if (insiv_received & run_mo != 5) {
    df_primera_status_email <- df_framework_primera_activation_status
  } else if (!insiv_received & run_mo != 5) {
    df_primera_status_email <- df_ecmwf_primera_activation_status %>%
      filter(adm0_es != "Guatemala")
  } else if (run_mo == 5) {
    df_primera_status_email <- df_ecmwf_primera_activation_status
  }
}
if(is_postrera){  
  if (insiv_received & run_mo != 9) {
    df_postrera_status_email <- df_ecmwf_postrera_activation_status
  } else if (!insiv_received & run_mo != 9) {
    df_postrera_status_email <- df_ecmwf_postrera_activation_status %>%
      filter(adm0_es != "Guatemala")
  } else if (run_mo == 9) {
    df_postrera_status_email <- df_ecmwf_postrera_activation_status
  }
}

if(is_primera){
  df_thresholds_email <- df_primera_status_email
  season <- "Primera"
}
if(is_postrera){
  df_thresholds_email <- df_postrera_status_email
  season <- "Postrera"
}



## 6b. Generate text used in email ####
email_txt <- email_text_list(
  df = df_thresholds_email,
  season = season,
  run_date = run_date,
  insiumeh_forecast_available = insiv_received
)

## 6c. Generate Status Table ####
df_thresholds_email <- df_thresholds_email %>% 
  mutate(
    adm0_es = fct_relevel(adm0_es, "El Salvador","Honduras","Nicaragua","Guatemala"),
    status = fct_expand(status, "No Activation","Activation")
  )

gt_threshold_table <- df_thresholds_email %>% 
  gt() %>% 
  cols_label(
    adm0_es="Country",
    value= "Rainfall (mm)",
    status = "Status",
    q_val = "Threshold"
  ) %>% 
  gt::cols_hide(
    columns = any_of(c('window',"lt","rp","q","status_lgl","source","include","forecast_source","pub_date"))
  ) %>% 
  gt::fmt_number(columns= c("value","q_val"),decimals=0) %>% 
  gt::tab_header(
    "Predicted Primera Rainfall and Trigger Thresholds"
  ) %>% 
  gt::tab_footnote(
    footnote = email_txt$tbl_footnote
  )
gdf_adm0_status <- adm0_simp %>% 
  left_join(
    df_thresholds_email
  ) 





## 6d. Generate Map - Choropleth ####
m_choro <- trigger_status_choropleth(gdf_adm0 = gdf_adm0_status,
                                     gdf_adm1 = adm1_simp,
                                     insivumeh_data_available = insiv_received,
                                     aoi_txt_label_size = 8,
                                     gdf_adm0_surrounding = adm0_surrounding_simp, run_date = run_date
)

## 6e. Generate plot ####
p_rainfall <- df_thresholds_email %>% 
  ggplot(
    aes(x= adm0_es, y= value), 
    width =0.2
  )+
  geom_point(
    aes(
      color=status,
    ) ,
    show.legend = c(color=TRUE)
  ) +
  scale_color_manual(
    values = c(
      `No Activation`="#55b284ff",
      `Activation` =hdx_hex("tomato-hdx")
    ),
    drop=F
  ) +
  geom_hline(
    aes(
      yintercept= q_val), 
    linetype="dashed",
    color="tomato"
  )+
  scale_y_continuous(
    limits=c(0,max(df_thresholds_email$value)),
    expand = expansion(mult = c(0,0.1))
  )+
  facet_wrap(
    ~adm0_es,
    scales = "free_x",
    nrow = 1,ncol=4
  )+
  labs(
    title = "CADC Drought Monitoring- Forecasted Primera Rainfall (MJJA 2024)",
    subtitle= glue("Forecast Published: 2024 {email_txt$month_chr}") ,
    y= "Rainfall (mm)",
    caption = "Horizonal red dashed lines indicate trigger threshold level."
  )+
  theme(
    axis.title.x = element_blank(),
    title = element_text(size=16),
    plot.subtitle = element_text(size=16),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    axis.text.y = element_text(angle=90,size=145),
    strip.text = element_text(size= 16),
    axis.text.x = element_blank(),
    plot.caption = element_text(hjust=0, size =14)
  )



## 6f. Render and send email #####

email_rmd_fp <- "email_cadc_drought_monitoring.Rmd"
# Load in e-mail credentials
email_creds <- creds_envvar(
  user = Sys.getenv("CHD_DS_EMAIL_USERNAME"),
  pass_envvar = "CHD_DS_EMAIL_PASSWORD",
  host = Sys.getenv("CHD_DS_HOST"),
  port = Sys.getenv("CHD_DS_PORT"),
  use_ssl = TRUE
)


# # so dont render by accident
# to = df_email_receps$Email,
render_email(
  input = email_rmd_fp,
  envir = parent.frame()
) %>%
  smtp_send(
    from = "data.science@humdata.org",
    to = "zachary.arno@un.org",
    subject = "AA Test",
    credentials = email_creds
  )

# email_txt$subj
# to = df_email_receps$Email,
