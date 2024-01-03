
aggregate_ecmwf_historical <- function(dir_ecmwf,
                                       init_trimester_month=c(5,6,9),
                                       zonal_boundary){
  fp_tif <- list.files(dir_ecmwf,pattern = "tif$",full.names = T)
  cat("loading rasters into memory\n")
  lr <- fp_tif %>% 
    map(\(fp_temp){
      fp_temp_base <- basename(fp_temp)
      file_date <-  as_date(str_extract(fp_temp_base,"\\d{4}-\\d{2}-\\d{2}"))
      rtmp <- rast(fp_temp)
      add_months<- (1:nlyr(rtmp))-1
      valid_date <- add_months %>% 
        map(
          ~file_date+ months(.x)
        ) %>% 
        unlist() %>% 
        as_date()
      
      rtmp %>% 
        set.names(valid_date)
      return(rtmp)
      
    })
  
  cat("filtering rasters\n")
  lr_filt <- lr %>% 
    map(
      \(rtmp){
        init_trimester_month %>% 
          map(\(tmp_valid_init_mo){
            # tmp_valid_init_mo<-init_trimester_month[1]
            monitoring_window <- seq(tmp_valid_init_mo-3,tmp_valid_init_mo)
            trigger_window <- seq(tmp_valid_init_mo, tmp_valid_init_mo+2)
            
            
            r_nm_tmp <- names(rtmp)
            
            if(month(r_nm_tmp[1]) %in% monitoring_window){
              r_ret <- rtmp[[month(r_nm_tmp) %in% trigger_window]]
            }else{
              r_ret <- NULL
            }
            return(r_ret)
            
          }) %>% 
          discard(is_empty)
      }
    ) %>%
    discard(is_empty) %>% 
    flatten() %>% 
    map(
      \(r_3){
        file_source_tmp <- basename(sources(r_3))
        # cat(file_source_tmp,"\n")
        date_pub <- as_date(str_extract(file_source_tmp,"\\d{4}-\\d{2}-\\d{2}"))
        
        date_init_valid<- names(r_3)[1]
        lt <- month(date_init_valid) - month(date_pub)
        
        composite_name <-  paste0(date_pub, ".lt.",lt)
        rtmp_composite <- sum(r_3)
        rtmp_composite %>%
          set.names(composite_name)
        return(rtmp_composite)
        sum(r_3)
      }
    )
  
  r_composites <- rast(lr_filt)

  df_zs_w1_wide<- exact_extract(r_composites,
                                y=zonal_boundary,
                                append_cols= "adm0_es",
                                fun = c("mean","median")
  ) 
  df_zs_w1 <- df_zs_w1_wide %>% 
    pivot_longer(-adm0_es,
                 names_to = "stat",
                 values_to = "value"
    ) %>% 
    separate(stat,
             into = c("stat","date","lt_cat","lt"),
             sep = "\\.") %>% 
    select(-lt_cat) %>% 
    mutate(
      mm = (value * (60 * 60 * 24 * 30)) * 1000,
      date= as_date(date),
      lt = as.numeric(lt),
      init_valid_date = date + months(lt),
      init_year = year(init_valid_date)
    ) %>% 
    arrange(adm0_es,stat,init_valid_date, date)
  return(df_zs_w1)
}



all_historical_ecmwf_zonal <- function(dir_ecmwf,
                                         zonal_boundary){
  fp_tif<- list.files(dir_ecmwf,pattern = "tif$",full.names = T)
  cat("loading rasters into memory\n")
  lr <- fp_tif %>% 
    map(\(fp_temp){
      fp_temp_base <- basename(fp_temp)
      pub_date <-  as_date(str_extract(fp_temp_base,"\\d{4}-\\d{2}-\\d{2}"))
      rtmp <- rast(fp_temp)
      
      bnames <- paste0(pub_date,"_",names(rtmp))
      rtmp %>% 
        set.names(bnames)
      return(rtmp)
      
    })
  r_composites <- rast(lr)

  df_zs_w1_wide<- exact_extract(r_composites,
                                y=zonal_boundary,
                                append_cols= "adm0_es",
                                fun = c("mean","median")
  ) 
  df_zs_w1_wide %>% 
    pivot_longer(-adm0_es,
                 names_to = "stat",
                 values_to = "value"
    ) %>% 
    separate(stat,
             into = c("stat","pub_date","lt"),
             sep = "\\.|_") %>% 
    mutate(
      pub_date = ymd(pub_date),
      lt= parse_number(lt),
      valid_date= pub_date +months(lt-1),
      mm = (value * (60 * 60 * 24 * 30)) * 1000
    )
}

#' ecmwf_summarise_seasons
#' @details
#' Works for default windows of "primera" and "postera" and Copernicus Data Store Public SEAS51 data
#' **Note** when we get access to HRES, we will need to adjust line that creates `monitoring_mo_seq` as will have an extra
#' month of lead time data
#' **Note** -- sequencing of months would be disrupted/faulty if we start using months that extend into next year i.e (11,12,1,2,)
#' 
#' @param df data.frame with all ECMWF data aggregated to monthly precip values for each leadtime
#' @param window_list list containing numeric vector representing months

ecmwf_summarise_seasons <-  function(df=df_ecmwf_zonal_all,
                                    window_list= list("primera"=c(5,6,7,8),
                                                      "postera"=c(9,10,11))
                                                      ){
  df <- df %>% 
    mutate(
      lt= lt-1,
      valid_mo = month(valid_date)
    ) %>% 
    filter(
      # ECMWF data was also reduced to admin boundaries w/ median stat, let's remove it for now.
      stat== "mean"
    )

  window_list %>% 
    imap(\(valid_mo_seq, window_name){
      # valid_mo_seq <- c(9,10,11)
         monitoring_mo_seq <- (max(valid_mo_seq)-5 ): min(valid_mo_seq)
         df %>% 
           filter(
             valid_mo %in% valid_mo_seq,
             month(pub_date) %in% monitoring_mo_seq
           ) %>%
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
             # for example in March we have leadtimes 2 (March + 2 = May),3 (... June),4,5. 
             # Therefore when we sum those leadtime values we take min() so we get the leadtime to first month being aggregated
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


overall_activation_rates <-  function(df_ecmwf,
                                      df_q_summary,
                                      rp=c(3,4,5,6,7)){
  
  rp %>% 
    map(\(rp_tmp){
      df_ecmwf_historical_flagged <- left_join(
        df_ecmwf,
        df_q_summary %>% 
          filter(rp==rp_tmp),
        by = c("adm0_es","window","lt")
      ) %>% 
        mutate(
          flag= mm<= q_val
        )
      
      df_ecmwf_primera_post_postrera_joint <- df_ecmwf_historical_flagged %>% 
        group_by(adm0_es,
                 # window,
                 pub_date_yr= floor_date(pub_date,"year")
        ) %>% 
        summarise(
          flag = any(flag),
          .groups="drop_last"
        ) %>% 
        summarise(
          ar_all_seas= mean(flag),
          rp_all_seas = 1/ar_all_seas
        ) 
      
      
      df_ecmwf_joint_lt_flag_rate <- df_ecmwf_historical_flagged %>% 
        group_by(adm0_es,
                 window,
                 pub_date_yr= floor_date(pub_date,"year")
        ) %>% 
        summarise(
          flag = any(flag),
          .groups="drop_last"
        ) %>% 
        summarise(
          ar_all_lt= mean(flag),
          rp_all_lt = 1/ar_all_lt,.groups="drop"
        ) 
      df_ecmwf_joint_lt_flag_rate_wide <- df_ecmwf_joint_lt_flag_rate %>% 
        pivot_wider(names_from = window,
                    values_from=rp_all_lt:ar_all_lt) 
      
      left_join(
        df_ecmwf_joint_lt_flag_rate_wide,
        df_ecmwf_primera_post_postrera_joint
      ) %>% 
        mutate(
          rp_set = rp_tmp
        )
    }
    ) %>% 
    list_rbind()
}



pretty_table_ecmwf_activation_rates <- function(df,
                                                rp = c(3,4,6,7)){
  set_names(rp,paste0("rp",rp)) %>% 
    map(
      ~ df %>% 
    filter(rp_set==.x) %>% 
    select(
      adm0_es, 
      rp_all_lt_primera,everything()
    ) %>% 
    
    gt() %>% 
    cols_hide(
      columns =matches("ar|rp_set")
    ) %>%
    cols_label(
      rp_all_lt_primera="Primera",
      rp_all_lt_postera="Postrera",
      
      rp_all_seas = "Either Season",
      adm0_es= "country"
    ) %>% 
    tab_spanner(
      columns = contains("rp_all_lt"),
      label = "RP/Activation By Season"
    ) %>%
    fmt_number(decimals = 2) %>% 
    tab_header(title = glue("Approximate Return Period/Activation Rate When Threshold Set to {.x} year RP For Each Leadtime"))
    )
}





