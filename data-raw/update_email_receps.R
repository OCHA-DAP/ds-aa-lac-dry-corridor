#' New email list created here:
#' https://docs.google.com/spreadsheets/d/1QP72oYebfIFcaWBVb0xU0v5XHuNqRo7y/edit?gid=153042719#gid=153042719
#' 
#' It now exceeds 50 recipient email address limit. Therefore, we will need
#' to split in 2 and send 2 emails. 
#' **Script Objective** Therefore, this script just reads in the new table
#' and tags address two one of two groups.
#' 
#' **Special Instructions** if anyone was wanted to reproduce this scipt
#' they would have to download the google doc above to there working directory.
#' I don't forsee anyone needing to do that - **no review required**


box::use(
  dplyr[...],
  stringr,
  readxl, 
  janitor,
  cumulus
)


df_email_recep <- readxl$read_xlsx("email_recepients_cadc_trigger.xlsx")


# Just a bit of dirty wrangling to add an email grouping system.
# I figure best to try to group it so that people from the same country
# offices are on the same email chain
# putting regional/hq people w/ the Guatemala office group as that is where
# questions could arise because of the additional forecast source.

df_email_recep_processed <- df_email_recep |>
  mutate(
    
    country_extract1 = case_when(
      stringr$str_detect(tolower(Role), "salvador|slv")~ "El Salvador",
      stringr$str_detect(tolower(Role), "guat|gtm")~ "Guatemala",
      stringr$str_detect(tolower(Role), "hondura|hnd")~ "Honduras",
      stringr$str_detect(tolower(Role), "nica|nicaragua")~ "Nicaragua"
    ),
    country_extract2 = case_when(
      stringr$str_detect(tolower(Organization), "salvador|slv")~ "El Salvador",
      stringr$str_detect(tolower(Organization), "guat|gtm")~ "Guatemala",
      stringr$str_detect(tolower(Organization), "hondura|hnd")~ "Honduras",
      stringr$str_detect(tolower(Organization), "nica|nicaragua")~ "Nicaragua"
    ),
    country_extract3= ifelse(is.na(country_extract1),country_extract2,country_extract1),
    country_extract4 = case_when(
      Organization == "Start Network"~ "Guatemala",
      .default = country_extract3
    ), 
    email_group = case_when(
      country_extract4 == "Guatemala"~"A",
      country_extract4 != "Guatemala"~"B",
      Group == "both" ~"Both",
      is.na(country_extract4)~"A",
      .default = NA_character_
      
    )
    
  ) |>
  janitor$clean_names() |>
  select(name:full_list, email_group)


cumulus$blob_write(
  df_email_recep_processed,
  name = "ds-aa-lac-dry-corridor/framework_update_2025/202504_email_recepients_cadc_trigger_2025.csv"
)
