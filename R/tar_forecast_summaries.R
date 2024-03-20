
#' ecmwf_summarise_seasons
#' @details
#' Works for default windows of "primera" and "postera" and Copernicus Data Store Public SEAS51 data
#' **Note** when we get access to HRES, we will need to adjust line that creates `monitoring_mo_seq` as will have an extra
#' month of lead time data
#' **Note** -- sequencing of months would be disrupted/faulty if we start using months that extend into next year i.e (11,12,1,2,)
#' 
#' @param df data.frame with all ECMWF data aggregated to monthly precip values for each leadtime
#' @param window_list list containing numeric vector representing months

summarise_seasons <-  function(df=df_ecmwf_zonal_all,
                                    window_list= list("primera"=c(5,6,7,8),
                                                      "postera"=c(9,10,11)),
                                    forecast_source ="mars"
                                                      ){
  df <- df %>% 
    mutate(
      valid_mo = month(valid_date)
    ) %>% 
    filter(
      # ECMWF data was also reduced to admin boundaries w/ median stat, let's remove it for now.
      stat== "mean"
    )
  # MARS & INSUVIMEH last forecast is publication date + 6 months
  if(forecast_source %in% c("mars","insuvimeh")){
    max_lt <- 6  
  }
  # CDs is publication date + 5 months
  if(forecast_source=="cds"){
    max_lt <- 5
  }
  
  window_list %>% 
    imap(\(valid_mo_seq, window_name){
      
      df %>% 
        group_by(pub_date, adm0_es) %>%
        filter(all(valid_mo_seq %in% valid_mo), valid_mo %in% valid_mo_seq) %>% 
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
         
    ) %>% 
    list_rbind()

}

#' Title
#' @description
#' could actually be a potentially handy genreal function if I deal w/ 
#' annoying lazy eval required by `group_by()`
#' @return
#' @export
#'
#' @examples
grouped_quantile_summary <-  function(df,
                                      x="mm",
                                      grp_vars=c("adm0_es","window", "lt"),
                                      rps=c(1:10)){
   df %>% 
    group_by(
      !!!syms(grp_vars)
    ) %>% 
    reframe (
      rp = rps,
      q = 1/rp,
      q_val = quantile(!!sym(x), probs =1/rp),
      
    ) 
}


