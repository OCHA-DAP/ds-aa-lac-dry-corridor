box::use(
  dplyr[...],
  rlang[...],
  glue[...],
  tibble[...],
  cumulus,
  sf,
  geoarrow[...],
  lubridate,
  janitor,
  stats,
  logger
)

box::use(
  ../datasources/insivumeh
)


#' @export
load_relevant_forecasts <- function(
    df , 
    activation_moment ,
    pcodes,
    gdf_zone
){
  
  use_insivumeh <- "INSIVUMEH" %in% df$forecast_source
  season <- unique(df$window)
  
  months_to_aggregate <- switch(
    season,
    "primera"= 5:8,
    "postrera"= 9:11
  )
  
  logger$log_info("Loading SEAS5 forecast")
  df_seas5 <- load_seas5_forecast_window_weighted(
    df = df,
    activation_moment = activation_moment,
    pcodes = pcodes
  ) |> 
    mutate(
      forecast_source = "SEAS5"
    )
  
  
  if(use_insivumeh){
    logger$log_info("Loading INSIVUMEH Raster")
    r_insiv <- insivumeh$load_ncdf_blob_insiv(run_date = activation_moment)
    logger$log_info("Running INSIVUMEH Zonal Stats")
    dfz_insiv <- insivumeh$zonal_insivumeh(
      r =r_insiv,
      zone = gdf_zone
      ) |>
      mutate(
        iso3 ="GTM",
        forecast_source ="INSIVUMEH"
      )
    
    # something else
    # 
    # dfz_insiv <- insivumeh$load_zonal_insivumeh(
    #   run_date= activation_moment,
    #   zone = gdf_zone)
    # need to build in a check on seas5_aggregate() for when months are outside of df
    # cumulus$seas5_aggregate_forecast(dfz_insiv, value = "value",valid_months = 5:8,     by = c("iso3","issued_date","forecast_source"))
    dfz_insiv_aggregated <- cumulus$seas5_aggregate_forecast(
      dfz_insiv,
      value = "value",
      
      valid_months = months_to_aggregate,
      by = c("iso3","issued_date","forecast_source")) 
    
    df_forecast <- bind_rows(
      df_seas5,
      dfz_insiv_aggregated
    )
  }
  if(!use_insivumeh){
    df_forecast <- df_seas5
  }
  df_forecast
}


#' @export
load_seas5_forecast_window_weighted <- function(
    df, 
    activation_moment,
    pcodes
){
  df_seas5 <- load_seas5_forecast_window(
    df = df,
    activation_moment = activation_moment,
    pcodes = pcodes
  )
  df_weights <-  load_seas5_forecast_weights(pcodes = pcodes)
  
  df_seas5_weighted_mean <- df_seas5 |> 
    left_join(df_weights, by = c("iso3", "pcode")) |> 
    group_by(iso3, issued_date, leadtime, valid_month_label) |> 
    summarise(
      value = stats$weighted.mean (value, w =seas5_n_upsampled_pixels ),
      .groups="drop"
    )
  
  
  
}

load_seas5_forecast_weights <- function(pcodes){
  con <-  cumulus$pg_con()
  
  tbl_polys <- tbl(con,"polygon") |> 
    filter(
      adm_level == 1,
      iso3 %in% c("NIC","HND","SLV","GTM"),
      pcode %in% pcodes
    ) |> 
    select(pcode, iso3,adm_level,name, seas5_n_upsampled_pixels)
  
  tbl_polys |> 
    collect() |> 
    janitor$clean_names()
  
}

load_seas5_forecast_window <- function(    df , 
                                           activation_moment,
                                           pcodes
){
  season <- unique(df$window)
  
  months_to_aggregate <- switch(
    season,
    "primera"= 5:8,
    "postrera"= 9:11
  )
  
  con <- cumulus$pg_con()
  df_seas5 <- tbl(con,"seas5") |> 
    filter(
      adm_level == 1,
      pcode %in% pcodes,
      issued_date ==activation_moment
    ) |> 
    collect()
  
  df_seas5_mm <- df_seas5 |> 
    mutate(
      value = lubridate$days_in_month(valid_date)*mean
    )
  
  df_seas5 <- cumulus$seas5_aggregate_forecast(
    df_seas5_mm,
    value = "value",
    valid_months = months_to_aggregate,
    by = c("iso3","pcode","issued_date")
  ) |> 
    mutate(
      forecast_source = "SEAS5"
    )
}


#' @export
load_adm1_sf <- function(){
  cumulus$blob_read(
    container = "projects",
    name =  "ds-aa-lac-dry-corridor/framework_update_2025/gdf_cadc_adm1.parquet",
    as_data_frame =FALSE
  ) |> 
    sf$st_as_sf() |> 
    janitor$clean_names()
}


#' @export
load_threshold_table <- function(file_name="df_thresholds_seas5_insivumeh_adm1_refined.parquet", fallback_to_seas5= FALSE){
  df <- load_raw_threshold_table(file_name=file_name)
  if(!fallback_to_seas5){
    ret <- df |> 
      filter(
        # do not use SEAS5 for Guatemala -- only for leadtime 0
        !(iso3 == "GTM" & leadtime %in% c(1:4) & forecast_source == "SEAS5"),
        # these 2 months not being moniored
        !(issued_month_label %in% c("Feb","Sep")),
        !(issued_month_label == "May" & window =="postrera")
      )
  }
  if(fallback_to_seas5){
    ret <- df |> 
      filter(
        forecast_source == "SEAS5",
        !(issued_month_label %in% c("Feb","Sep")),
        !(issued_month_label == "May" & window =="postrera")
      )
    
  }
  ret
}


#' @export
load_raw_threshold_table <- function(file_name="df_thresholds_seas5_insivumeh_adm1_refined.parquet"){
  cumulus$blob_read(
    container = "projects",
    name = paste0("ds-aa-lac-dry-corridor/framework_update_2025/",file_name)
  )
}


#' RP based thresholding/classifications
#'
#' @param df 
#' @param var 
#' @param by 
#' @param rp_threshold 
#' @param direction 
#'
#' @returns
#' @export
threshold_var <-  function(df,var, by,rp_threshold,direction=1){
  if(direction==1){
    ret <- df |> 
      group_by(
        across({{by}})
      ) |> 
      arrange(
        desc(!!sym(var))
      ) |> 
      mutate(
        rank = row_number(),
        q_rank = rank/(max(rank)+1),
        rp_emp = 1/q_rank,
        !!sym(glue("{var}_flag")):= rp_emp>=rp_threshold
      ) |> 
      select(
        -rank,
        -q_rank
      )
  }
  if(direction == -1){
    ret <- df |> 
      group_by(
        across({{by}})
      ) |> 
      arrange(
        (!!sym(var))
      ) |> 
      mutate(
        rank = row_number(),
        q_rank = rank/(max(rank)+1),
        rp_emp = 1/q_rank,
        !!sym(glue("{var}_flag")):= rp_emp>=rp_threshold
      ) |> 
      select(
        -rank,
        -q_rank
      )
  }
  ret
}

#' @export
load_aoi_df <- function(version = c("2025_v2","2025_v1","startnetwork")){
  version <- arg_match(version)
  
  if(version=="2025_v1"){
    ret <- tribble(
      ~pcode, ~iso3,               ~name,
      "GT20", "GTM",        "Chiquimula",
      "HN07", "HND",        "El Paraiso",
      "HN08", "HND", "Francisco Morazan",
      "NI20", "NIC",            "Madriz",
      "NI25", "NIC",            "Estelí",
      "NI05", "NIC",     "Nueva Segovia",
      "NI40", "NIC",         "Matagalpa",
      "SV11", "SLV",       "San Vicente"
    )  
  }
  
  if(version=="2025_v2"){
    ret <- tribble(
      ~pcode, ~iso3,               ~name,
      "GT20", "GTM",        "Chiquimula",
      "HN07", "HND",        "El Paraiso",
      "HN08", "HND", "Francisco Morazan",
      # "NI20", "NIC",            "Madriz",
      # "NI25", "NIC",            "Estelí",
      # "NI05", "NIC",     "Nueva Segovia",
      # "NI40", "NIC",         "Matagalpa",
      "SV11", "SLV",       "San Vicente"
    )  
  }
  if(version == "startnetwork"){
    ret <- tribble(
      ~pcode, ~iso3,               ~name,
      "GT14", "GTM",        "Quiche",
      "GT15", "GTM",        "Baja Verapaz",
    )  
  }
  ret
}
