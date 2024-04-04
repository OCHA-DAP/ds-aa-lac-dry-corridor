#' Description
#' Script for monitoring CADC trigger.
#' It needs to be run manually as natl forecast pipeline is not predictable.
#' Unfortunately, no getting around this as we need to manually check this data everytime.
#' 
#' Some aspects, of script are being set for eventual migration to GH Action, but in other
#' components this approach was abandoned -- eventually could be refactored to GH Action if
#' data pipelines become stable.

library(aws.s3)
library(tidyverse)
library(sf)
library(googledrive)
library(exactextractr)
library(fs)
library(gt)
library(glue)
library(gghdx)
library(blastula)
library(here)
library(targets) # should remove for GHA eventually
gghdx()

fp_email_util_funcs <- list.files(
  file.path("src","email"),
  full.names = T
  )

walk(fp_email_util_funcs,~source(.x))

# just going to use these target functions to ease review since they've already been review
source(
  file.path("R","tar_forecast_summaries.R")
)
source(
  file.path("R","tar_insuvimeh.R")
)

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

# I started setting this up using just googledrive package to allow data to be passed over during 
# a GH Action, but it's currently a bit complicated by the uncertainty of INSIVUMEH data.
# This is why there is a mix of input sources/access methods ({googledrive}, googledrive file paths, AWS).
# Everything here works as long as you have you're googledrive synced, but if pipeline is ever made secure we can transfer 
# everything to Azure cloud

# authenticate w/ service account
drive_auth(path="sa_auth.json")

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
  file_name = "central_america_aoi_adm0.rds"
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


# file path where .nc files from INSIVUMEH are being saved
insiv_gdb <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "raw",
  "lac",
  "INSUVIMEH",
  "new_format")

# load shape files for zonal extractoin
targets::tar_load(gdf_aoi_adm)

# load thresholds -- includes all framework thresholds
tar_load(df_all_thresholds_rp4,store='_targets')


# 3. Get Relevant Thresholds ----------------------------------------------------------


# quick calculations to get the right lead time -- to filter to correct threshold. 
# need lead time & season

# publication_months for primera and postrera are mutually exclusive, but someone will likely ask
# for the May forecast of Postrera so will allow them to be evaluated independently regardless of month

get_relevant_threshold <- function(df_thresh_tbl,run_date,season_params){
  run_mo <- month(run_date)
  run_mo %in% season_params$pub_months
  
  start_mo <-  min(season_params$valid_months)
  lt_current <-  start_mo-run_mo
  # filter to correct threshold
  df_all_thresholds_rp4 %>% 
    filter(
      window == trigger_season,
      lt == lt_current
    )
}

get_relevant_threshold(df_thresh_tbl = df_all_thresholds_rp4,
                       run_date = Sys.Date(),
                       season_params =primera_params )



primera_monitoring <- run_mo %in% primera_params$pub_months
postrera_monitoring <- run_mo %in% postrera_params$pub_months

# Get thresholds for current season + publication month combinations
if(primera_monitoring){
  start_mo_primera <-  min(primera_params$valid_months)
  lt_primera <-  start_mo_primera-month(run_date)  
  # filter to correct threshold
  df_threshold_primera <- df_all_thresholds_rp4 %>% 
    filter(
      window == trigger_season,
      lt == lt_primera
    )
}
if(postrera_monitoring){
  start_mo_postrera <-  min(postrera_params$valid_months)
  lt_postrera <-  start_mo_postrera-month(run_date)  
  # filter to correct threshold
  df_threshold_postrera <- df_all_thresholds_rp4 %>% 
    filter(
      window == trigger_season,
      lt == lt_postrera
    )
}


# 4. Process Latest ECMWF ----------------------------------------------------

## 4a. Download ECMWF raster for AOI ####
# Remember we've already processed the ECMWF raster to nice tidy global COGs and  placed
# them in an AWS S3 directory with `src/download_process_latest_ecmwf_grib.R`


# from gdal documentation about vsi on s3
url_raster <- paste0(
  "/vsis3/", # critical parameter
  Sys.getenv("BUCKET_NAME"),
  "/ECMWF_COGS/",
  run_date_chr,
  "-ECMWF_SEAS-V_i_a-ensemble_mean.tif"
)

# this downloads the selected AOI from global raster stored in S3 bucket
fp_raster <- file.path(
  tempdir(),
  paste0(
    run_date_chr,
    "_ecmwf_seas.tif"
  )
)
sf::gdal_utils(
  "warp",
  source = url_raster,
  destination = fp_raster,
  options = c(
    "-t_srs", st_crs(gdf_aoi_adm$adm0)$wkt,
    "-te", sf::st_bbox(gdf_aoi_adm$adm0)
  )
)

## 4b. Zonal Stats on ECMWF raster ####

r_ecmwf <- terra::rast(
  fp_raster
)
df_ecmwf_monthly <- exact_extract(x = r_ecmwf,
                                  y=gdf_aoi,
                                  fun="mean",
                                  append_cols="adm0_es"
) %>% 
  pivot_longer(-matches("adm0_")) %>%
  separate(name, into = c("stat", "pub_date", "lt"), sep = "\\.|_") %>%
  # add season information by combing dates w/ leadtimes
  mutate(
    pub_date = as_date(pub_date),
    lt = parse_number(lt),
    valid_date = pub_date + months(lt)
  )

## 4c. Summaries monthly values to relevant season ####

if(primera_monitoring){
  
  df_ecmwf_primera_sums <-  summarise_forecasted_season(
    df =df_ecmwf_monthly,
    season_params = primera_params 
  ) %>% 
    mutate(
      forecast_source = "ECMWF MARS"
    )
  ## 4d. compare to relevant thresholds and upate status based on purely ECMWF ####
  df_ecmwf_primera_activation_status <- left_join(
    df_ecmwf_primera_sums,
    df_threshold_primera
  ) %>% 
    mutate(
      status_lgl= value<q_val,
      status= if_else(value<q_val,"Activation","No Activation"),
      status= fct_expand(status,"Activation","No Activation"),
      source= "ECMWF"
    )
  
}

# repeat for postrera
if(postrera_monitoring){
  df_ecmwf_postrera_sums <-  summarise_forecasted_season(
    df =df_ecmwf_monthly,
    season_params = postrera_params 
  ) %>% 
    mutate(
      forecast_source = "ECMWF MARS"
    )
  df_ecmwf_prostrera_activation_status <- left_join(
    df_ecmwf_postrera_sums,
    df_threshold_postrera
  ) %>% 
    mutate(
      status_lgl= value<q_val,
      status= if_else(value<q_val,"Activation","No Activation"),
      status= fct_expand(status,"Activation","No Activation"),
      source= "ECMWF"
    )
}

# 5. INSIVUMEH DATA ----------------------------------------------------------

# INSIVUMEH Data is used for Guatemala only for all months except when pub_month = valid_month (i.e it's May and we are monitoring May in Primera)

## 5a. check if INSIVUMEH Data has been received ####

# Data pipeline for receiving INSIVUMEH is not predicatable, so this function just checks if it's been received by checking the
# the directory I store it in.
insiv_received <- insivumeh_received(gdb_base = insiv_gdb,
                                     run_date = run_date)

# if Insiv received run the following calculations
if(insiv_received){
  
  ## 5b. load & process insivumeh raster ####
  r_insiv <-  load_insuvimeh_raster(
    # build_insiv_path -- just builds correct path based on date and INSIVUMEH directory
    # sharing structure
    gdb =build_insiv_path(gdb_base =insiv_gdb,run_date = run_date ),
    wrap = F
  )
  
  ## 5c. Zonal Stats on INSIVUMEH raster ####
  gdf_gtm <- gdf_aoi_adm$adm0 %>% 
    filter(adm0_pcode=="GT")
  
  df_monthly_gtm <- zonal_gtm_insuvimeh(
    r = r_insiv,
    gdf = gdf_aoi_adm,
    rm_dup_years = F # left over from targets pipelines, dup_years issue fixed in raw data.
  )
  
  ## 5d. Summaries monthly values to relevant season ####
  # for May we have to use ECMWF.
  if(primera_monitoring & run_mo!=5){
    df_insiv_primera_sums <-  summarise_forecasted_season(
      df =df_monthly_gtm,
      season_params = primera_params 
    ) %>% 
    mutate(
      forecast_source = "INSUVIMEH"
    )
  
  ## 5e. update threshold activation status with insivumeh forecast ####
  insiv_status_update <- left_join(
    df_insiv_primera_sums,
    df_threshold_primera
  ) %>% 
    mutate(
      status_lgl= value<q_val,
      status = ifelse(status_lgl,"Activation","No Activation"),
      source = "INSIVUMEH"
    )
  
  df_primera_all_forecasts_status <- bind_rows(df_ecmwf_primera_activation_status,
                                               insiv_status_update)
  activation_rows <- filter(df_all_forecasts_status,status_lgl)
  
  if(nrow(activation_rows)==0){
    cat("no thresholds breached from ECMWF or INSIV")
  }
  
  ## 5f. remove status for Guatemala ####
  df_framework_primera_activation_status <- df_primera_all_forecasts_status %>% 
    mutate(
      include = ifelse(source=="ECMWF" & adm0_es =="Guatemala",F,T),
      status= fct_expand(status,"Activation","No Activation")
    ) %>% 
    filter(include) %>% 
    ungroup()
  } # end primera monitoring if
  
  # Repeat for Postrera
  
  if(postrera_monitoring ){
    df_insiv_postrera_sums <-  summarise_forecasted_season(
      df =df_monthly_gtm,
      season_params = postrera_params 
    ) %>% 
      mutate(
        forecast_source = "INSUVIMEH"
      )
    
    ## 5e. update threshold activation status with insivumeh forecast ####
    insiv_postrera_status_update <- left_join(
      df_insiv_postrera_sums,
      df_threshold_postrera
    ) %>% 
      mutate(
        status_lgl= value<q_val,
        status = ifelse(status_lgl,"Activation","No Activation"),
        source = "INSIVUMEH"
      )
    
    df_postrear_all_forecasts_status <- bind_rows(df_ecmwf_postrera_activation_status,
                                                 insiv_postrera_status_update)
    activation_rows <- filter(df_postrera_all_forecasts_status,status_lgl)
    
    if(nrow(activation_rows)==0){
      cat("no thresholds breached from ECMWF or INSIV")
    }
    
    ## 5f. remove status for Guatemala ####
    df_framework_postrera_activation_status <- df_postrera_all_forecasts_status %>% 
      mutate(
        include = ifelse(source=="ECMWF" & adm0_es =="Guatemala",F,T),
        status= fct_expand(status,"Activation","No Activation")
      ) %>% 
      filter(include) %>% 
      ungroup()
  } # end postrera monitoring if statement
  
} # end insiv

# 6. Generate email graphics -------------------------------------------------

## 6a. Choose data.frame to base email & graphics ####

# if we have insivumeh and it's not May we can use INSIVUMEH
if(insiv_received &  run_mo!=5){
  df_status_email <- df_framework_primera_activation_status
} 

# If don't have INSIVUMEH and it's not May, we should use ECMWF
# but remove Guatemala

if(!insiv_received & run_mo!=5){
  df_status_email <- df_ecmwf_primera_activation_status %>% 
    filter(adm0_es!="Guatemala")
}

# No matter what if it's May we have to use only ECMWF.
if(run_mo==5){
  df_status_email <- df_ecmwf_primera_activation_status 
}

## 6b. Generate text used in email ####
email_txt <- email_text_list(
  df = df_status_email,
  season = "Primera",
  run_date = run_date,
  insiumeh_forecast_available = insiv_received
)

## 6c. Generate Status Table ####
gt_threshold_table <- df_status_email %>% 
  gt() %>% 
  cols_label(
    adm0_es="Country",
    value= "Rainfall (mm)",
    status = "Status",
    q_val = "Threshold"
  ) %>% 
  gt::cols_hide(
    columns = any_of(c('window',"lt","rp","q","status_lgl","source","include","forecast_source"))
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
    df_activation_status
  )

## 6d. Generate Map - Choropleth ####
m_choro <- trigger_status_choropleth(gdf_adm0 = gdf_adm0_status,
                                     gdf_adm1 = adm1_simp,
                                     insivumeh_data_available = insiv_received,
                                     aoi_txt_label_size = 8,
                                     gdf_adm0_surrounding = adm0_surrounding_simp
)

## 6e. Generate plot ####
p_rainfall <- df_status_email %>% 
  ggplot(
    aes(x= adm0_es, y= value), 
    width =0.2
  )+
  geom_point(
    aes(
      color=status,
    ) 
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
    limits=c(0,max(df_activation_status$value)),
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
    axis.text.y = element_text(angle=90,size=14),
    strip.text = element_text(size= 16),
    axis.text.x = element_blank(),
    plot.caption = element_text(hjust=0, size =12)
  )


## 6f. Render and send email #####
# Load in e-mail credentials
email_creds <- creds_envvar(
  user = Sys.getenv("CHD_DS_EMAIL_USERNAME"),
  pass_envvar = "CHD_DS_EMAIL_PASSWORD",
  host = Sys.getenv("CHD_DS_HOST"),
  port = Sys.getenv("CHD_DS_PORT"),
  use_ssl = TRUE
)
email_rmd_fp <- "email_cadc_drought_monitoring.Rmd"

# so dont render by accident
# render_email(
#   input = email_rmd_fp,
#   envir = parent.frame()
# ) %>%
#   smtp_send(
#     to = df_email_receps$Email,
#     from = "data.science@humdata.org",
#     subject = email_txt$subj,
#     credentials = email_creds
#   )  
