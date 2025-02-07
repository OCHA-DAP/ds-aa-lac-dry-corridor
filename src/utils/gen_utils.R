box::use(
  dplyr[...],
  rlang[...],
  glue[...],
  tibble[...],
  cumulus,
  sf,
  geoarrow[...]
)


#' @export
load_adm1_sf <- function(){
  cumulus$blob_read(
    container = "projects",
    name =  "ds-aa-lac-dry-corridor/framework_update_2025/gdf_cadc_adm1.parquet",
    as_data_frame =FALSE
  ) |> 
    sf$st_as_sf()
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
load_aoi_df <- function(version = "2025_v1"){
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
  ret
}
