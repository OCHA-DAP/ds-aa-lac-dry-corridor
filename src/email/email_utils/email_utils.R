
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
  html <- blastula::add_image(
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
add_ggplot_custom <- function(
    plot_object,
    width = 5,
    height = 5,
    html_width = 1000,
    alt = NULL,
    align = c("center", "left", "right", "inline"),
    float = c("none", "left", "right")
) {
  html <- blastula::add_ggplot(
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

# add_gt_custom <- function(
    #     plot_object,
#     width = 5,
#     height = 5,
#     html_width = 1000,
#     alt = NULL,
#     align = c("center", "left", "right", "inline"),
#     float = c("none", "left", "right")
# ) {
#   html <- blastula::add_ggplot(
#     plot_object = plot_object,
#     width = width,
#     height = height,
#     alt = alt,
#     align = align,
#     float = float
#   )
#
#   custom_html(
#     html = html,
#     width = html_width
#   )
# }

custom_html <- function(html, width) {
  img_html <- stringr::str_extract(
    html,
    "(<img src.*px;\"/>)",
    group = 1
  )
  img_html_styled <- stringr::str_replace(
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

add_tmap <- function (plot_object,
                      width = 5,
                      height = 5,
                      alt = NULL,
                      align = c("center",
                                "left",
                                "right",
                                "inline"),
                      float = c("none", "left", "right")
)
{
  tmpfile <- tempfile("tmap", fileext = ".png")
  if (requireNamespace("tmap", quietly = TRUE)) {
    tmap::tmap_save(tm = plot_object,
                    # device = "png",
                    filename = tmpfile,
                    dpi = 200,
                    width = width,
                    height = height)
  }
  else {
    stop("Please ensure that the `tmap` package is installed before using `add_tmap()`.",
         call. = FALSE)
  }
  on.exit(file.remove(tmpfile), add = TRUE)
  alt_text <- alt
  image_html <- add_image(file = tmpfile,
                          alt = alt_text,
                          width = width * 100,
                          align = align,
                          float = float)
  image_html
}


add_tmap_custom <-  function(
    plot_object,
    width = 5,
    height = 5,
    html_width = 1000,
    alt = NULL,
    align = c("center", "left", "right", "inline"),
    float = c("none", "left", "right")
){
  html <- add_tmap(
    plot_object = plot_object,
    width = width,
    height = height,
    alt = alt,
    align = align,
    float = float
  )
  # return(html)
  custom_html(
    html = html,
    width = html_width
  )
}


# google drive helpers ----------------------------------------------------


#' Title
#'
#' @param dribble 
#' @param file_name 
#'
#' @return
#' @export
#'
#' @examples
load_drive_file <-  function(
    dribble,
    file_name){
  file_id <-  get_drive_id(dribble = dribble,
                           file_name = file_name)
  tmp_file_path <-  file.path(
    tempdir(),file_name
  )
  
  cat("downloading ", file_name," to temp path\n")
  googledrive::drive_download(
    file = googledrive::as_id(file_id),
    path = tmp_file_path
  )
  cat("reading ", file_name," to memory\n")
  ret <- read_fun(tmp_file_path)
  unlink(tmp_file_path)
  return(ret)
}

#' Title
#'
#' @param dribble 
#' @param file_name 
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' get_drive_id(
#'   dribble = drive_dribble,
#'   file_name = "central_america_aoi_adm0.rds" 
#' )
#' }
get_drive_id <- function(dribble, file_name){
  dribble %>% 
    dplyr::filter(
      name == file_name
    ) %>% 
    dplyr::pull(id) %>% 
    googledrive::as_id()
}





#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
read_fun <- function(x="central_america_aoi_adm0.rds"){
  file_ext<- fs::path_ext(x)
  switch(file_ext,
         "rds"=readr::read_rds(x),
         "csv"= readr::read_csv(x)
  )
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

email_text_list <- function(df=df_activation_status,
                        season = "Primera",
                        run_date= run_date,
                        insiumeh_forecast_available
                        ){
  
  
  df_filt_activations <- df %>% 
    filter(
      status_lgl
    )
  

  description_ending <-  ifelse(!insiumeh_forecast_available,
                                "The trigger status and thresholds are based on the latest ECMWF Seasonal forecast and historical ECMWF Seasonal forecasts for each country independently. <br><br><i>As indicated in the Dry Corridor AA framework, for Guatemala, the final trigger status is determined using the forecast of the national meteorological service INSIVUMEH. Therefore, the trigger status for Guatemala will be provided when the INSIVUMEH forecast is received which is estimated to be between the 5th and 10th of the month.</i>",
                                "The trigger status and thresholds are based on the latest ECMWF Seasonal forecast and historical ECMWF Seasonal forecasts for Nicarauga, El Salvador, and Honduras independently. For Guatemala the thresholds and trigger as based on the official national forecast and historical forecasts obtained from INSIVUMEH.")
  
  table_footnote <- ifelse(
    !insiumeh_forecast_available ,
    glue("Thresholds for all countries have been calculated from historical ECMWF (1981-2022) to approximate 4 year return period drought level. An update will be provided for Guatemala when the national forecast data are received."),
    "Thresholds calculated to approximate a 4 year return period drought level. For Guatemala, these calculations were based on the official national historical forecasts from INSIVUMEH (1981-2022), For the remaining 3 countries the calculations were based on historical seasonal ECMWF Forecasts (1981-2022)"
  )
  
  subj_ending <-  ifelse(!insiumeh_forecast_available,"(NIC,HND,SLV)","(NIC, HND, SLV,GTM)")
  month_chr <- as.character(month(run_date,
                     abbr=F,
                     label = T))
  subj_month <- ifelse(!insiumeh_forecast_available,glue("Preliminary {month_chr}"),month_chr)
  
  
  
  date_header <- glue("{format(run_date,'%d %B %Y')} - Trigger status:")
  if(nrow(df_filt_activations)==0){
    
    description_contents_txt= glue("The AA framework has has not been triggered in any country. The total rainfall forecast over the 2024 {season} season (May-August) is not predicted to be below the 1 in 4 year drought levels. {description_ending}")
    # date_header_txt = glue("{date_header_prefix}:")
    subj_status <-  "No Activations"
    trigger_status_txt= "<span style='color: #55b284ff;'>Not Activated</span>"
  }
  
  if(nrow(df_filt_activations)>0){ 
    subj_status <- "Activated"
    countries_activated_txt <- glue_collapse(df_filt_activations$adm0_es,sep = ",",last = " & ")
    description_contents_txt= glue("The AA framework has been triggered in {countries_activated_txt} where the combined rainfall forecast over the 2024 {season} season (May-August) is predicted to below the 1 in 4 year drought levels. {description_ending}")
    trigger_status_txt = "<span style='color: #F2645A;'>Activated</span>"
  }
  list(
    month_chr = month_chr,
    subj = glue("AA Central America Dry Corridor - Drought Monitoring - {subj_month} update - {subj_status} {subj_ending}"),
    title = "Anticipatory Action- Central American Dry Corridor",
    subtitle = glue("2024 {season} Drought Monitoring - {month_chr} Update"),
    date_header = date_header,
    status=trigger_status_txt,
    description_title = "Trigger Description",
    description_content = description_contents_txt,
    contact_info= "Contact the OCHA Centre for Humanitarian Data via Leonardo Milano, Team Lead\nfor Data Science at leonardo.milano@un.org with any questions or feedback.",
    tbl_footnote = table_footnote,
    data_source = ifelse(insiumeh_forecast_available,"ECMWF SEAS51 & INSUVIMEH","ECMWF SEAS51"),
    ref_github = "Full documentation and source code can be found in the [GitHub repository](https://github.com/OCHA-DAP/ds-aa-lac-dry-corridor) and [Technical note](https://data.humdata.org/dataset/2048a947-5714-4220-905b-e662cbcd14c8/resource/35031e9a-37eb-4566-915c-cff18b3cc3d9/download/chd_cadc_drought_trigger_technical_note_2024.pdf)"
    )
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

#' build_insiv_path
#' helper function to create path to hypothetical folder containing latest INSIVUMEH  naming convention based on run_date/pub_date
#' can deprecate as we move from gdrive
#' @param gdb_base 
#' @param run_date 
#'
#' @return
#' @export
#'
#' @examples
build_insiv_path <-  function(gdb_base,run_date){
  DIR_CURRENT_INSIV <- paste0("start",month(run_date,abbr = T,label = T))
  
  file.path(
    gdb_base,
    year(run_date),
    DIR_CURRENT_INSIV
  )
}


