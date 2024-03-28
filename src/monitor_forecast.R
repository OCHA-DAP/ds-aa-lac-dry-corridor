# monitor trigger
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

source(
  file.path("src","email","email_utils.R")
)
source(
  file.path("R","monitoring_viz.R")
)
source(
  file.path("R","tar_forecast_summaries.R")
)
source(
  file.path("R","tar_insuvimeh.R")
)

# Run date is critical as it sets the publiation month
run_date <-  Sys.Date()
run_date_chr <-  format(floor_date(run_date, "month"),"%Y%m")
run_mo <-  month(run_date)

# load shape files for zonal extractoin
targets::tar_load(gdf_aoi_adm)

# load thresholds -- includes all framework thresholds
tar_load(df_all_thresholds_rp4,store='_targets')


primera_params <- list(
  pub_months = c(3,4,5),
  valid_months = c(5,6,7,8)
)
postrera_params <- list(
  pub_months = c(6,7,8),
  valid_months = c(9,10,11)
)

# quick calculations to get the right lead time -- to filter to correct threshold - 
# need lead time & season

# lead time
start_mo_primera <-  min(primera_params$valid_months)
lt_primera <-  start_mo_primera-month(run_date)

# season
trigger_season <-  case_when(
  run_mo %in% c(3,4,5)~"primera",
  run_mo %in% c(6,7,8)~"postera",
  .default = NA
)

# filter to correct threshold
df_threshold <- df_all_thresholds_rp4 %>% 
  filter(
    window == trigger_season,
    lt == lt_primera
  )


insiv_gdb <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "raw",
  "lac",
  "INSUVIMEH",
  "new_format")

# if new insivimeh forecast provided gab and process it
insiv_received <- insivumeh_received(gdb_base = insiv_gdb,
                                     run_date = run_date)
if(insiv_received){
  r_insiv <-  load_insuvimeh_raster(
    gdb =build_insiv_path(gdb_base =insiv_gdb,run_date = run_date ),
    wrap = F
  )
  gdf_gtm <- gdf_aoi_adm$adm0 %>% 
    filter(adm0_pcode=="GT")
  df_monthly_gtm <- zonal_gtm_insuvimeh(
    r = r_insiv,
    gdf = gdf_aoi_adm,
    rm_dup_years = F
  )
  
  df_insiv_primera_sums <- df_monthly_gtm %>% 
    filter(
      month(pub_date) %in% primera_params$pub_months,
      month(valid_date) %in% primera_params$valid_months
    ) %>% 
    group_by(adm0_es,pub_date) %>% 
    summarise(
      value = sum(value)
    ) %>% 
    mutate(
      forecast_source = "INSUVIMEH"
      )
 
  
  
  insiv_update <- left_join(
    df_insiv_primera_sums,
    df_threshold
    ) %>% 
    mutate(
      status_lgl= value<q_val,
      status = ifelse(status_lgl,"Activation","No Activation")
    )
  
}

# get drive files ---------------------------------------------------------
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


# get ECMWF SEAS

# from gdal documentation about vsi on s3
url_raster <- paste0(
  "/vsis3/", # critical parameter
  Sys.getenv("BUCKET_NAME"),
  "/ECMWF_COGS/",
  run_date_chr,
  "-ECMWF_SEAS-V_i_a-ensemble_mean.tif"
)


# this downloads the selected AOI from S3
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

df_ecmwf_primera_sums <- df_ecmwf_monthly %>% 
  filter(
    month(pub_date) %in% primera_params$pub_months,
    month(valid_date) %in% primera_params$valid_months
  ) %>% 
  group_by(adm0_es,pub_date) %>% 
  summarise(
    value = sum(value)
  ) %>% 
  mutate(forecast_source="ECMWF MARS")

df_ecmwf_postrera_sums <- df_ecmwf_monthly %>% 
  filter(
    month(pub_date) %in% postrera_params$pub_months,
    month(valid_date) %in% postrera_params$valid_months
  ) %>% 
  group_by(adm0_es,pub_date) %>% 
  summarise(
    value = sum(value)
  )

if(nrow(df_ecmwf_primera_sums)>0){
  df_ecmwf_activation_status <- left_join(
    df_ecmwf_primera_sums,
    df_threshold
    ) %>% 
    mutate(
      status_lgl= value<q_val,
      status= if_else(value<q_val,"Activation","No Activation"),
      status= fct_expand(status,"Activation","No Activation"),
      source= "ECMWF"
    )
    if(insiv_received){
      df_all_forecasts_status <- bind_rows(df_ecmwf_activation_status,
                                        insiv_update %>% mutate(source = "INSIVUMEH")) 
      activation_rows <- filter(df_all_forecasts_status,status_lgl)
      if(nrow(activation_rows)==0){
        cat("no activations any rows")
      }
      
      df_activation_status <- df_all_forecasts_status %>% 
        mutate(
          include = ifelse(source=="ECMWF" & adm0_es =="Guatemala",F,T),
          status= fct_expand(status,"Activation","No Activation")
        ) %>% 
        filter(include) %>% 
        ungroup()
    }
  
  email_txt <- email_text_list(
    df = df_activation_status,
    season = "Primera",
    run_date = run_date,
    prelim= !insiv_received
  )
  
  
  gt_threshold_table <- df_activation_status %>% 
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
  

    
  m_choro <- trigger_status_choropleth(gdf_adm0 = gdf_adm0_status,
                            gdf_adm1 = adm1_simp,
                            insivumeh_data_available = insiv_received,
                            aoi_txt_label_size = 8,
                            gdf_adm0_surrounding = adm0_surrounding_simp
                            )
  
  if(insiv_received){
    df_plot <- df_activation_status
  }  
  if(!insiv_received){
    df_plot <-  df_activation_status %>% 
      filter(adm0_es!="Guatemala")
    
  }
  
  p_rainfall <- df_plot %>% 
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
}


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
