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
gghdx()
source(
  file.path(
    "src",
    "email",
    "email_utils.R"
  )
)
source(
  file.path(
    "R",
    "monitoring_viz.R"
  )
)

run_date <-  Sys.Date()
run_date_chr <-  format(floor_date(run_date, "month"),"%Y%m")

run_mo <-  month(run_date)

trigger_season <-  case_when(
  run_mo %in% c(3,4,5)~"primera",
  run_mo %in% c(6,7,8)~"postera",
  .default = NA
  )

# Until INSUVMEH - CHD data pipeline is clarified it remains somewhat unstable so cannot be built into pipeline/GH Actions
# Unclear if it will be single tifs, nc files, or something else. Therefore, for now we will run it in the _targets pipeline
# and run some manual checks -- then send the analyzed summarised data.frame to the GH Action folder 
preliminary_results <- c(T,F)[2]
new_format_gtm_data <- c(T,F)[1]
if(!preliminary_results){
  
  
  
  if(new_format_gtm_data){
    targets::tar_load(df_insuvimeh_trigger_status_new)
    insuv_update <- df_insuvimeh_trigger_status_new
  }
  if(!new_format_gtm_data){
    targets::tar_load(df_insuvimeh_trigger_status)
    insuv_update <- df_insuvimeh_trigger_status
  }
  
  new_insuvimeh_integrated <- insuv_update$pub_date == floor_date(run_date,"month")
  if(new_insuvimeh_integrated){
    df_insuv_simp <- insuv_update %>% 
      
      mutate(
        window="primera",
        source= 'INSUVIMEH',
      ) %>% 
      select(adm0_es, value, lt, window, rp, q, q_val,  status,status_lgl,source)
    
    
    # df_insuvimeh_trigger_status really needs to read from gdrive instead of targets
    
    # df_activation_status %>% 
    #   colnames()
    # gha_dir <- fs::path(
    #   Sys.getenv("AA_DATA_DIR"), 
    #   "..",
    #   "..",
    #   "ROLAC",
    #   "Dry Corridor",
    #   "CADC_GHA" 
    # )
    
  }
}

tar_load(df_all_thresholds_rp4,store='_targets')

targets::tar_load(df_mars_q_summary)

primera_params <- list(
  pub_months = c(3,4,5),
  valid_months = c(5,6,7,8)
)
postrera_params <- list(
  pub_months = c(6,7,8),
  valid_months = c(9,10,11)
)




start_mo_primera <-  min(primera_params$valid_months)
lt_primera <-  start_mo_primera-month(run_date)


df_all_thresholds_rp4 %>% 
  filter(
    window == "primera",
    lt == lt_primera
  )
# May is LT 0
# April is LT 1
# March is LT 2
# Fe would be LT 3


df_threshold <- df_mars_q_summary %>% 
  filter(
    rp==4,
    window =="primera",
    lt==lt_primera
    )



targets::tar_load(gdf_aoi_adm)


# get drive files ---------------------------------------------------------
drive_auth(path="sa_auth.json")
drive_dribble <- drive_ls(
  corpus= "user"
)

df_email_receps <- load_drive_file(
  dribble= drive_dribble,
  file_name = "email_recepients_cadc_trigger.csv"
)


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

df_primera_sums <- df_ecmwf_monthly %>% 
  filter(
    month(pub_date) %in% primera_params$pub_months,
    month(valid_date) %in% primera_params$valid_months
  ) %>% 
  group_by(adm0_es) %>% 
  summarise(
    value = sum(value)
  )
df_postrera_sums <- df_ecmwf_monthly %>% 
  filter(
    month(pub_date) %in% postrera_params$pub_months,
    month(valid_date) %in% postrera_params$valid_months
  ) %>% 
  group_by(adm0_es) %>% 
  summarise(
    value = sum(value)
  )

if(nrow(df_primera_sums)>0){
  df_activation_status <- left_join(
    df_primera_sums,
    df_threshold
    ) %>% 
    # mutate(
    #   q_val = 940
    # ) %>% 
    mutate(
      status_lgl= value<q_val,
      status= if_else(value<q_val,"Activation","No Activation"),
      status= fct_expand(status,"Activation","No Activation"),
      source= "ECMWF"
    )
    if(!preliminary_results){
      df_activation_status <- bind_rows(df_activation_status,
                df_insuv_simp) %>% 
        mutate(
          include = ifelse(source=="ECMWF"&adm0_es =="Guatemala",F,T),
          status= fct_expand(status,"Activation","No Activation")
        ) %>% 
        filter(include)
    }
  
  email_txt <- email_text_list(
    df = df_activation_status,
    season = "Primera",
    run_date = run_date,
    prelim=preliminary_results
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
      columns = any_of(c('window',"lt","rp","q","status_lgl","source","include"))
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
                            aoi_txt_label_size = 8,
                            gdf_adm0_surrounding = adm0_surrounding_simp
                            )
  
  p_rainfall <- df_activation_status %>% 
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

render_email(
  input = email_rmd_fp,
  envir = parent.frame()
) %>%
  smtp_send(
    to = df_email_receps$Email,
    from = "data.science@humdata.org",
    subject = email_txt$subj,
    credentials = email_creds
  )  
