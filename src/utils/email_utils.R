
box::use(
  dplyr[...],
  dplyr,
  gghdx, 
  lubridate[...],
  glue[...],
  stringr,
  blastula,
  cumulus,
  janitor,
  rlang
)
# gghdx$gghdx()

#' @export
load_email_recipients <- function(email_list){
  ret <- load_raw_email_recipients() |> 
    filter_recepients(
      email = email_list
    )
  if(email_list == "full_list"){
    ldf_email_split_raw <- split(ret,ret$email_group) 
    ret <- list(
      group_a = bind_rows(ldf_email_split_raw$A,ldf_email_split_raw$Both),
      group_b = bind_rows(ldf_email_split_raw$B,ldf_email_split_raw$Both)
    )  
  }
  ret
}

filter_recepients <-  function(df,email_list){
  df |> 
    select(
    all_of(c(
        "name", "organization", "role", "email","email_group",email_list
      )
      )
    ) |> 
    filter(!is.na(!!rlang$sym(email_list)))
}

load_raw_email_recipients <- function(){
  df_email_receps <- cumulus$blob_read(
    # name = "ds-aa-lac-dry-corridor/framework_update_2025/email_recepients_cadc_trigger_2025.csv",
    name = "ds-aa-lac-dry-corridor/framework_update_2025/202504_email_recepients_cadc_trigger_2025.csv",
    container = "projects",
    stage = "dev"
  ) |> 
    janitor$clean_names()
}




# Email Text --------------------------------------------------------------


#' Title
#'
#' @param df 
#' @param season 
#' @param run_date 
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' email_text_list(
#'   season= "Primera",
#'   run_date = Sys.Date()
#'   )}

email_text_list <- function(df,
                            run_date,
                            insivumeh_forecast_available
){
  
  
  df_filt_activations <- df %>% 
    filter(
      status_lgl
    )
  
  season <- stringr$str_to_title(unique(df$window))
  
  
  description_ending <-  gen_description_end(run_date = run_date,insivumeh_forecast_available = insivumeh_forecast_available)
  table_footnote <- gen_table_footnote(run_date = run_date,insivumeh_forecast_available = insivumeh_forecast_available)
  
  # subj_ending <-  ifelse(!insivumeh_forecast_available,"(NIC,HND,SLV)","(NIC, HND, SLV,GTM)")
  month_chr <- as.character(month(run_date,
                                  abbr=F,
                                  label = T))
  monitored_range <- ifelse(season == "Primera","May-August","September-November")
  
  plot_title <- ifelse(season=="Primera",
                       "CADC Drought Monitoring- Forecasted Primera Rainfall (MJJA 2025)",
                       "CADC Drought Monitoring- Forecasted Postrera Rainfall (SON 2025)")
  # subj_month <- ifelse(!insivumeh_forecast_available,glue("Preliminary {month_chr}"),month_chr)
  
  
  
  date_header <- glue("{format(run_date,'%e %B %Y')} - Trigger status:")
  if(nrow(df_filt_activations)==0){
    
    description_contents_txt= glue("The AA framework has not triggered in any country. The total rainfall forecast over the 2025 {season} season ({monitored_range}) is not predicted to be below the 1 in 4 year drought levels. {description_ending}")
    # subj_status <-  "No Activations"
    trigger_status_txt= "<span style='color: #55b284ff;'>Not Activated</span>"
  }
  
  if(nrow(df_filt_activations)>0){ 
    # subj_status <- "Activated"
    countries_activated_txt <- glue_collapse(df_filt_activations$adm0_es,sep = ",",last = " & ")
    description_contents_txt= glue("The AA framework has been triggered in {countries_activated_txt} where the combined rainfall forecast over the 2025 {season} season ({monitored_range}) is predicted to below the 1 in 4 year drought levels. {description_ending}")
    trigger_status_txt = "<span style='color: #F2645A;'>Activated</span>"
  }
  list(
    month_chr = month_chr,
    subj = gen_subject(df = df,run_date = run_date,insivumeh_forecast_available=insivumeh_forecast_available),
    title = "Anticipatory Action- Central American Dry Corridor",
    subtitle = glue("2025 {season} Drought Monitoring - {month_chr} Update"),
    plot_title = plot_title,
    gt_table_header = glue("Predicted {stringr$str_to_title(season)} Rainfall and Trigger Thresholds"),
    date_header = date_header,
    status=trigger_status_txt,
    description_title = "Trigger Description",
    description_content = description_contents_txt,
    contact_info= "Contact the OCHA Centre for Humanitarian Data via Leonardo Milano, Team Lead\nfor Data Science at leonardo.milano@un.org with any questions or feedback.",
    tbl_footnote = table_footnote,
    data_source = ifelse(insivumeh_forecast_available,"ECMWF SEAS5 & INSIVUMEH","ECMWF SEAS5"),
    ref_github = "Full documentation and source code can be found in the [GitHub repository](https://github.com/OCHA-DAP/ds-aa-lac-dry-corridor)."
  )
}

#' Title
#'
#' @param run_date 
#' @param insivumeh_forecast_available 
#'
#' @return
#'
#' @examples \dontrun{
#' gen_description_end(run_date = Sys.Date(),insivumeh_forecast_available = insiv_received)
#' }

gen_description_end <- function(run_date,insivumeh_forecast_available){
  run_mo <- month(run_date)
  if(run_mo==5) {
    ret <- "The trigger status and thresholds are based on the latest ECMWF Seasonal forecast and historical ECMWF Seasonal forecasts for each country independently.<br><br><i>As indicated in the Dry Corridor AA framework, the May update only uses ECMWF data which allows for the inclusion of May rainfall forecast in the calculations.</i>"
  }
  else if (run_mo!=5){
    ret <-  ifelse(!insivumeh_forecast_available,
                   "The trigger status and thresholds are based on the latest ECMWF Seasonal forecast and historical ECMWF Seasonal forecasts for each country independently. <br><br><i>As indicated in the Dry Corridor AA framework, for Guatemala, the final trigger status is determined using the forecast of the national meteorological service INSIVUMEH. Therefore, the trigger status for Guatemala will be provided when the INSIVUMEH forecast is received which is estimated to be between the 5th and 10th of the month.</i>",
                   "The trigger status and thresholds are based on the latest ECMWF Seasonal forecast and historical ECMWF Seasonal forecasts for El Salvador and Honduras independently. For Guatemala the thresholds and trigger as based on the official national forecast and historical forecasts obtained from INSIVUMEH.")
    
  }
  return(ret)
}


#' Title
#'
#' @param run_date 
#' @param insivumeh_forecast_available 
#'
#' @return
#'
#' @examples \dontrun{
#'  gen_table_footnote(run_date = Sys.Date(),insivumeh_forecast_available = insiv_received)
#' }
gen_table_footnote <- function(run_date,insivumeh_forecast_available){
  run_mo <- month(run_date)
  if(run_mo==5) {
    ret <- "Thresholds for all countries have been calculated from historical ECMWF (1981-2022) to approximate 4 year return period drought level."
    
  }
  else if (run_mo!=5){
    ret <- ifelse(
      !insivumeh_forecast_available ,
      glue("Thresholds for all countries have been calculated from historical ECMWF (1981-2022) to approximate 4 year return period drought level. An update will be provided for Guatemala when the national forecast data are received."),
      "Thresholds calculated to approximate a 4 year return period drought level. For Guatemala, these calculations were based on the official national historical forecasts from INSIVUMEH (1981-2022), For the remaining 2 countries the calculations were based on historical seasonal ECMWF Forecasts (1981-2022)"
    )
  }
  return(ret)
}

gen_subject <- function(df,
                        run_date,
                        insivumeh_forecast_available
){
  run_mo <- month(run_date)
  df_filt <- df %>% 
    filter(
      status_lgl
    )
  
  month_chr <- as.character(month(run_date,
                                  abbr=F,
                                  label = T))
  
  
  subj_status <- ifelse(nrow(df_filt)>0,"Activated","No Activations")
  
  if(run_mo==5|insivumeh_forecast_available){
    subj_ending <- "(HND, SLV, GTM)"
    subj_month <- month_chr
  }
  else if(!insivumeh_forecast_available){
    subj_ending <- "(HND, SLV)"
    subj_month <- glue("Preliminary {month_chr}")
  }
  ret <- glue("AA Central America Dry Corridor - Drought Monitoring - {subj_month} update - {subj_status} {subj_ending}")
  return(ret)
}

#' insivumeh_received
#' @details
#' helper function to check if the INSIVUMEH forecast has been received and put in the correct
#' location for the pipeline.
#' 
#' @param gdb_base `character` file path to base directory the sub-directories are based on how INSIVUMEH has shared the 
#'   data an follow the syntax of file.path(gdb_base, YYYY, start{month(pub_date, abbr=T, label=T)})
#' @param run_date `date` date of run -- will get converted/floored to month downstream
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' run_date <- Sys.Date()
#' insiv_gdb <- file.path(
#'     Sys.getenv("AA_DATA_DIR"),
#'     "private",
#'     "raw",
#'     "lac",
#'     "INSUVIMEH",
#'     "new_format")
#'            
#'insivumeh_received(gdb_base = insiv_gdb,
#'                   run_date = run_date)
#'}
insivumeh_received <-  function(gdb_base,run_date= run_date){
  
  # create path to hypothetical folder using naming convention based on run_date/pub_date
  DIR_INSIV <- build_insiv_path(gdb_base = gdb_base,
                                run_date = run_date)
  
  cat("checking ",basename(DIR_INSIV), " for 6 new forecast files\n")
  
  # let user know if 6 unique forecast files files exist or not.
  FILENAMES_INSIV <- list.files(DIR_INSIV)
  filenames_unique <-  unique(FILENAMES_INSIV)
  num_unique_files <- length(FILENAMES_INSIV)
  
  ret_lgl <- num_unique_files==6
  if(ret_lgl){
    cat("6 unique INSIVUMEH files found for current run month")
  }else{
    cat("6 unique INSIVUMEH files NOT FOUND for current month")
  }
  return(ret_lgl)
  
}


# blastula/html wrappers --------------------------------------------------


# utility functions for the emails

# creates a custom add image by using code that makes the image responsive
# even in Windows Outlook
# based on here: https://stackoverflow.com/questions/2426072/is-there-an-equivalent-of-css-max-width-that-works-in-html-emails
add_image_custom <- function(
    file,
    alt = "",
    width = 520,
    align = c("center", "left", "right", "inline"),
    float = c("none", "left", "right")
) {
  # get default blastula image HTML
  html <- blastula$add_image(
    file = file,
    alt = alt,
    width = width,
    align = align,
    float = float
  )
  
  custom_html(
    html = html,
    width = width
  )
}

# use add_image to save out plots and add them back to the file
# use custom width
#' @export
add_ggplot_custom <- function(
    plot_object,
    width = 5,
    height = 5,
    html_width = 1000,
    alt = NULL,
    align = c("center", "left", "right", "inline"),
    float = c("none", "left", "right")
) {
  html <- blastula$add_ggplot(
    plot_object = plot_object,
    width = width,
    height = height,
    alt = alt,
    align = align,
    float = float
  )
  
  custom_html(
    html = html,
    width = html_width
  )
}


custom_html <- function(html, width) {
  img_html <- stringr$str_extract(
    html,
    "(<img src.*px;\"/>)",
    group = 1
  )
  img_html_styled <- stringr$str_replace(
    img_html,
    "(?<=style=\")(.*)(?=\"/>)",
    "display:block;width:100%"
  )
  
  # create the new custom table for the HTML
  cat(
    paste0(
      '<table border="0" cellspacing="0" width="100%"><tr><td></td><td width="',
      width,
      '">',
      img_html_styled,
      "</td><td></td></tr></table>"
    )
  )
}