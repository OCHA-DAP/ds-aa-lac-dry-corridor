#' Nicaragua was removed from AA framework, but we are still monitoring
#' it informally. We are not sending any framework emails, but 
#' rather we can create customized tables.
#' 
#' I just basically copied the script from src/monoitorin_2025/update_activation_status.R
#' and modified it slightly... Will leave in some of the email generation
#' code in case we decide to spruce this into a real email. Currently
#' we just share w/ screenshots of tables w/ AA focal point for further
#' communication

box::use(
  dplyr[...],
  forcats[...],
  glue[...],
  purrr[...],
  readr,
  stringr,
  tidyr,
  cumulus,
  lubridate,
  logger,
  sf,
  gt,
  gghdx,
  ggplot2[...],
  gghdx,
  blastula[...],
  geoarrow[...],
  
)

gghdx$gghdx()

box::use(
  utils = ../utils/gen_utils,
  eu=../utils/email_utils,
  ../datasources/insivumeh,
  ../utils/map
)

WHEN_TO_MONITOR_LOCAL_DEFAULT <- c("last_primera","last_postrera","current")[3]
EMAIL_WHO_LOCAL_DEFAULT <- c("core_developer","developers","interna_chd","internal_ocha","full_list")[1]


logger$log_info(paste0("EMAIL_WHO = ", Sys.getenv("EMAIL_WHO")))
logger$log_info(paste0("WHEN_TO_MONITOR = ", Sys.getenv("WHEN_TO_MONITOR")))

EMAIL_LIST <- Sys.getenv("EMAIL_WHO", unset = EMAIL_WHO_LOCAL_DEFAULT)

monitoring_when <-   Sys.getenv("WHEN_TO_MONITOR", unset = WHEN_TO_MONITOR_LOCAL_DEFAULT)
run_date_set <- case_when(
  monitoring_when == "last_primera" ~ lubridate$as_date("2024-04-05"),
  monitoring_when == "last_postrera" ~ lubridate$as_date("2024-06-05"),
  monitoring_when == "current" ~ Sys.Date(),
)


logger$log_info(paste0("EMAIL_LIST = ", EMAIL_LIST))
logger$log_info(paste0("Run date set = ", run_date_set))

# df_email_receps <- eu$load_email_recipients(email_list = EMAIL_LIST)


current_moment <-  lubridate$floor_date(run_date_set, "month")

insiv_received <- insivumeh$insivumeh_availability(run_date = current_moment)


# Loading base data -------------------------------------------------------

# this function can/should be edited to reflect changes in monitoring AOI
# version is the critical parameter to monitor nica
df_aoi <- utils$load_aoi_df(version = "2025_v1")
gdf_adm1 <- utils$load_adm1_sf()

df_admin_name_lookup <- cumulus$blob_load_admin_lookup()

# this threshold table dictates which thresholds and forecast source we use.
df_thresholds <- utils$load_threshold_table(
  file_name="df_thresholds_seas5_insivumeh_adm1_refined.parquet", 
  fallback_to_seas5 = FALSE
)


# Minor filtering/wrangling -----------------------------------------------

df_relevant_thresholds <- df_thresholds |> 
  filter(
    issued_month_label == lubridate$month(current_moment, abbr= T, label =T)
  )


gdf_adm1_aoi <-  gdf_adm1 |> 
  filter(
    adm1_pcode %in% df_aoi$pcode
  ) 


gdf_aoi_country <- gdf_adm1_aoi |> 
  group_by(adm0_es) |> 
  summarise(do_union = T)


gdf_aoi_gtm <- gdf_aoi_country |> 
  filter(adm0_es == "Guatemala")


# Loading forecasts -------------------------------------------------------

logger$log_info("Getting lastest SEAS5 forecast from Postgres")
# box::reload(insivumeh)
# box::reload(utils)

if(!insiv_received){
  df_relevant_thresholds <- df_relevant_thresholds |> 
    filter(
      forecast_source != "INSIVUMEH"
    )
}
df_forecast <-  utils$load_relevant_forecasts(
  df = df_relevant_thresholds,
  activation_moment = current_moment,
  pcodes = df_aoi$pcode,
  gdf_zone= gdf_aoi_gtm # this is only used for INSIVUMEH
)

# Assessing activation ####

df_forecast_status <- df_forecast |> 
  # inner_join() will only keep the INSIVUMEH when needed
  inner_join(df_relevant_thresholds) |> 
  mutate(
    status_lgl = value<= value_empirical,
    status = if_else(status_lgl, "Activation","No Activation"),
    status = fct_expand(factor(status),"Activation","No Activation")
  )



# Preparing email content -------------------------------------------------
# box::reload(eu)
email_txt <- eu$email_text_list(
  df = df_forecast_status,
  run_date = run_date_set,
  insivumeh_forecast_available = insiv_received 
)

logger$log_info("Making threshold table")
gt_threshold_table <- df_forecast_status |> 
  filter(adm0_es == "Nicaragua") |> 
  select(
    adm0_es,value,value_empirical,status
  ) |> 
  gt$gt() |> 
  gt$cols_hide(columns = "status") |> 
  gt$cols_label(
    adm0_es="Country",
    value= "Rainfall (mm)",
    status = "Status",
    value_empirical = "Threshold"
  ) |> 
  gt$fmt_number(columns= c("value","value_empirical"),decimals=0) |> 
  gt$tab_header(
    email_txt$gt_table_header
  ) |> 
  gt$tab_footnote(
    footnote = "Thresholds have been calculated from historical ECMWF (1981-2022) to approximate 4 year return period drought level."
  ) |> 
  gt$tab_options(
    table.font.size = 14,
    heading.background.color = "#55b284ff",
    # table.width = px(500)
    table.width = gt$pct(80)
  )
gt_threshold_table

logger$log_info("Making admin AOI table")
gt_aoi <- gdf_adm1_aoi |> 
  
  sf$st_drop_geometry() |> 
  filter(adm0_es == "Nicaragua") |> 
  group_by(
    adm0_es
  ) |> 
  summarise(
    admin_1 = glue_collapse(adm1_es, sep = ", ")
  ) |> 
  gt$gt() |> 
  gt$cols_label(
    adm0_es = "Country",
    admin_1 = "Admin 1"
  ) |> 
  gt$tab_header(
    title = "Admin 1 units included in monitoring by country"
  ) |> 
  gt$cols_align(
    align = "left"
  ) |> 
  gt$tab_options(
    heading.background.color = "#55b284ff",
    column_labels.background.color = "#D2F2F0",
    table.font.size = 14,
    table.width = gt$pct(80)
  )





gdf_adm0_status <- gdf_aoi_country |>
  left_join(
    df_forecast_status |> 
      select(adm0_es,status)
  )

logger$log_info("Loading Map layers from blob")
l_gdf_simple <-  map$load_simplified_map_layers()

# Move the row where adm0_pcode is "NI" from AOI_ADM0 to AOI_SURROUNDING
# ni_row <- l_gdf_simple$AOI_ADM0 %>%
#   filter(adm0_pcode == "NI")
# 
# # Remove the row from AOI_ADM0
# l_gdf_simple$AOI_ADM0 <- l_gdf_simple$AOI_ADM0 %>%
#   filter(adm0_pcode != "NI")

# Add the row to AOI_SURROUNDING
# l_gdf_simple$AOI_SURROUNDING <- bind_rows(l_gdf_simple$AOI_SURROUNDING, ni_row)


# box::reload(map)
# ## 6d. Generate Map - Choropleth ####
logger$log_info("Making Map")
m_choro <- map$trigger_status_choropleth(
  gdf_aoi = gdf_adm0_status, # dissolved admin file
  gdf_adm1 = l_gdf_simple$AOI_ADM1, # full country admin 1
  gdf_adm0_surrounding = l_gdf_simple$AOI_SURROUNDING, # surrounding
  gdf_adm0 = l_gdf_simple$AOI_ADM0, # full country admin 0,
  insivumeh_data_available = insiv_received, # automate
  aoi_txt_label_size = 8,
  run_date = run_date_set
)


logger$log_info("make rainfall plot")
## 6e. Generate plot ####
p_rainfall <- df_forecast_status |> 
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
      `Activation` ="#F2645A"
    ),
    drop=F
  ) +
  geom_hline(
    aes(
      yintercept= value_empirical), 
    linetype="dashed",
    color="tomato"
  )+
  scale_y_continuous(
    limits=c(0,max(df_forecast_status$value)),
    expand = expansion(mult = c(0,0.1))
  )+
  facet_wrap(
    ~adm0_es,
    scales = "free_x",
    nrow = 1,ncol=4
  )+
  labs(
    title = email_txt$plot_title,
    subtitle= glue("Forecast Published: 2025 {email_txt$month_chr}") ,
    y= "Rainfall (mm)",
    caption = "Horizonal red dashed lines indicate trigger threshold level."
  )+
  theme(
    axis.title.x = element_blank(),
    title = element_text(size=16),
    plot.subtitle = element_text(size=16),
    legend.title = element_blank(),
    legend.text = element_text(size=15),
    axis.text.y = element_text(angle=90,size=15),
    strip.text = element_text(size= 16),
    axis.text.x = element_blank(),
    plot.caption = element_text(hjust=0, size =14)
  )


# 
# 
# email_rmd_fp <- "email_cadc_drought_monitoring_2025.Rmd"
# 
# # Load in e-mail credentials
# email_creds <- creds_envvar(
#   user = Sys.getenv("CHD_DS_EMAIL_USERNAME"),
#   pass_envvar = "CHD_DS_EMAIL_PASSWORD",
#   host = Sys.getenv("CHD_DS_HOST"),
#   port = Sys.getenv("CHD_DS_PORT"),
#   use_ssl = TRUE
# )
# 
# logger$log_info("knitting email")
# # in worse case that this won't send in next step - you can print this 
# # object and click `export` in Rstudio to get the .html which can be shred
# 
# knitted_email <- render_email(
#   input = email_rmd_fp,
#   envir = parent.frame()
# )
# 
# 
# logger$log_info("Sending email")
# 
# ldf_email_use |> 
#   map(\(dfet){
#     dfet$Email
#     smtp_send(
#       email = knitted_email,
#       from = "data.science@humdata.org",
#       to = dfet$Email,
#       # to = "zachary.arno@un.org",
#       # subject = ifelse(EMAIL_LIST!="full_list",paste0("TEST: ",email_txt$subj),email_txt$subj),
#       subject = email_txt$subj,
#       credentials = email_creds,
#       verbose = TRUE
#     )
#     
#   })
