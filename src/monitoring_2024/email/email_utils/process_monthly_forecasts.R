
merge_forecast_status <-  function(df_ecmwf_status, df_insiv_status){
  
  df_all_status <- bind_rows(df_ecmwf_status,
                             df_insiv_status)
  activation_rows <- filter(df_all_status,status_lgl)
  
  if(nrow(activation_rows)==0){
    cat("no thresholds breached from ECMWF or INSIV")
  }
  
  ## 5f. remove status for Guatemala ####
  df_all_status %>% 
    mutate(
      include = ifelse(source %in% c("ECMWF","ECMWF MARS","ECMWF CDs") & adm0_es =="Guatemala",F,T),
      status= fct_expand(status,"Activation","No Activation")
    ) %>% 
    filter(include) %>% 
    ungroup()
}



#' Title
#'
#' @param df_threshold 
#' @param df_forecast_monthly 
#' @param season_params 
#' @param forecast_source 
#'
#' @return
#' @export
#'
#' @examples
process_monthly_forecast <- function(df_threshold, 
                                     df_forecast_monthly,
                                     season_params,
                                     forecast_source) {
  summarise_forecasted_season(
    df = df_forecast_monthly,
    season_params = season_params
  ) %>%
    mutate(forecast_source = forecast_source) %>%
    left_join(df_threshold) %>%
    mutate(
      status_lgl = value < q_val,
      status = if_else(value < q_val, "Activation", "No Activation"),
      status = fct_expand(status, "Activation", "No Activation"),
      source = forecast_source
    )
}

#' Title
#'
#' @param df 
#' @param season_params 
#'
#' @return
#' @export
#'
#' @examples
#' 

summarise_forecasted_season <- function(df, season_params){
  df %>% 
    filter(
      month(pub_date) %in% season_params$pub_months,
      
      # all month of monitored season must be present in forecast valid_date
      all(season_params$valid_months %in% month(valid_date)),
      month(valid_date) %in% season_params$valid_months
    ) %>% 
    group_by(adm0_es,pub_date) %>% 
    summarise(
      value = sum(value),.groups="drop"
    ) 
}


get_relevant_threshold <- function(df_thresh_tbl, run_date, season_params) {
  run_mo <- month(run_date)
  start_mo <- min(season_params$valid_months)
  lt_current <- start_mo - run_mo
  df_thresh_tbl %>%
    filter(
      window == season_params$trigger_season, 
           lt == lt_current)
}


