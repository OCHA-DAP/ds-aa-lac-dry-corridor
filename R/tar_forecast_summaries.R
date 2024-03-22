library(tidyverse)

#' summarise_seasons
#' @details
#' Custom function for _targets pipeline to summaries forecast data
#' by publication date to get total rainfall quantity predicted at specific
#' publication dates/lead times for a provided window/set of valid months. The
#' window_list argument allows you to supply multiple windows which can be overlapping or not.
#' 
#' **Note** -- function is only for this _targets pipeline and not meant to be generic. It
#' is currently set to run on the desired window periods for the framework and could be disrupted/faulty if
#' we start using months that expand across multiple years i.e (11,12,1,2.). This will be adapted more generally in the triggerApp
#' with unit tests.
#' 
#' @param df `data.frame` containing monthly rainfall forecast
#'   required cols:
#'     "adm0_es": `character` country name
#'     "pub_date": `publication date of forecast
#'     "lt": `numeric` leadtime
#'     "mm": `numeric` mm rain predicted for the month/lt combination
#'     "valid_date": `date` containing valid date constructed from pub_date + lubridate::months(lt) 
#' @param window_list named list containing numeric vector valid months for consideration in window and the name of list 
#'   representing the name to call the window period
#' @examples \dontrun{
#' library(targets)
#' library(tidyverse)
#' tar_load(df_ecmwf_mars)
#' df_ecmwf_mars_prepped <- df_ecmwf_mars %>% 
#'   rename(mm="value") 
#'  
#' summarise_seasons(
#'   df_ecmwf_mars_prepped,
#'   window_list= list("primera"=c(5,6,7,8),
#'                      "postera"=c(9,10,11))
#'   )
#' }

summarise_seasons <-  function(df,
                               window_list= list("primera"=c(5,6,7,8),
                                                 "postera"=c(9,10,11))
){
  df <- df %>% 
    mutate(
      valid_mo = month(valid_date)
    ) %>% 
    filter(
      # ECMWF data was also reduced to admin boundaries w/ median stat, let's remove it for now.
      stat== "mean"
    )
  
  imap(window_list,\(valid_mo_seq_tmp, window_name_tmp){
    sum_forecast_windows(df = df,
                valid_months = valid_mo_seq_tmp,
                window_name = window_name_tmp)
  }) %>% 
    list_rbind()
}

#' sum_forecast_windows
#' @description
#' Custom function for _targets pipeline to summaries forecast data
#' by publication date to get total rainfall quantity predicted at specific
#' publication dates/lead times for a provided window/set of valid months.
#' 
#' **Note:** this just a helper function to improve legibility/clarity of`summarise_seasons()` which loops through
#' different windows of interest. It is not meant to be generic.
#'
#' @param df `data.frame` containing monthly rainfall forecast
#'   required cols:
#'     "adm0_es": `character` country name
#'     "pub_date": `publication date of forecast
#'     "lt": `numeric` leadtime
#'     "mm": `numeric` mm rain predicted for the month/lt combination
#'     "valid_mo": `integer` containing valid month constructed from pub_date + lubridate::months(lt) 
#' @param valid_months `integer` vector containing month considered as valid for the window of interest
#' @param window_name `character` name to use for window. Will be added as column
#'
#' @return `data.frame` with rainfall (mm) with the following columns: c("pub_date", "adm0_es", "mm", "lt", "window")
#'   where mm is summarized per pub_date and `lt` represents the minimum leadtime to the start of the valid window

#' @examples \dontrun{
#' library(targets)
#' library(tidyverse)
#' tar_load(df_ecmwf_mars)
#' df_ecmwf_mars_prepped <- df_ecmwf_mars %>% 
#'   rename(mm="value") %>% 
#'   mutate(
#'   valid_mo = month(valid_date)
#'   ) %>% 
#'   filter(
#' # ECMWF data was also reduced to admin boundaries w/ median stat, let's remove it for now.
#'   stat== "mean"
#'   )
#'   
#' sum_forecast_windows(df_ecmwf_mars_prepped,valid_months = c(5,6,7,8),window_name="Primera")
#'   
#' }
sum_forecast_windows <-  function(df,
                         valid_months,
                         window_name){
  df %>% 
    group_by(pub_date, adm0_es) %>%
    filter(all(valid_months %in% valid_mo), valid_mo %in% valid_months) %>% 
    arrange(
      adm0_es, pub_date, lt
    ) %>% 
    group_by(pub_date,adm0_es) %>%
    mutate(
      count =length(unique(lt))
    ) %>% 
    summarise(
      mm = sum(mm),
      # **min() - BECAUSE**  for MJJA (5,6,7,8) at each pub_date we have a set of leadtimes
      # for EXAMPLE in March we have the following leadtimes 2 
      # 2 : March + 2 = May,
      # 3 : March + 3 = June,
      # 4 : March + 4 = July
      # 5:  March + 5 = Aug
      # Therefore when we sum those leadtime precip values we take min() of lt integer so we get the leadtime to first month being aggregated
      lt= min(lt),
      .groups = "drop"
    ) %>% 
    mutate(
      window = window_name
    )
}

#' grouped_quantile_summary
#' @description
#' Given a data frame with a numeric vector this function can provide percentile breaks based on 
#' user defined grouping variables and a sequence of custom return periods to be converted to percentile
#' @param df `description `data.frame`
#' @param x `character` column name of `numeric` vector that we want reclassified to percentiles
#' @param grp_vars `character` vector containing column names to group "x" by when reclassifying by percentile
#' @param rp `numeric` vector containing the return periods of interest - will be converted to percentiles by 1/rp.
#' @return `data.frame` with original columns except x. Instead of x the data.frame has contains:
#'   `rp` (return period)
#'   `q` (percentile)
#'   `q_val`  (percentile break)
#' rows added for every combination of group vars and rps provided.
#' @examples \dontrun{
#' library(targets)
#' library(tidyverse)
#' tar_load(df_ecmwf_mars)
#' df_ecmwf_mars_prepped <- df_ecmwf_mars %>% 
#'   rename(mm="value") 
#'  
#' df_summarized <- summarise_seasons(
#'   df_ecmwf_mars_prepped,
#'   window_list= list("primera"=c(5,6,7,8),
#'                      "postera"=c(9,10,11))
#'   )
#'   grouped_quantile_summary(
#'     df= df_summarized ,
#'     x = "mm",
#'     rps = c(1:10),
#'     grp_vars=c("adm0_es","window", "lt")
#'   )
#' }
grouped_quantile_summary <-  function(df,
                                      x="mm",
                                      grp_vars=c("adm0_es","window", "lt"),
                                      rps=c(1:10)){
   df %>% 
    group_by(
      across(all_of(grp_vars))
    ) %>% 
    reframe (
      rp = rps,
      q = 1/rp,
      q_val = quantile(.data[[x]], probs =1/rp),
      
    ) 
}


