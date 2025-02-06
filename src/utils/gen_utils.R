box::use(
  dplyr[...],
  rlang[...],
  glue[...],
  tibble[...]
)


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
