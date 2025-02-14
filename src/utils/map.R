box::use(
  ggplot2[...],
  sf[...],
  dplyr[...],
  lubridate[...],
  forcats[...],
  gghdx,
  cumulus,
  purrr
)
#' Title
#'
#' @param gdf_adm0 
#' @param gdf_adm1 
#'
#' @return
#' @export
#'
#' @examples
trigger_status_choropleth <- function(
    gdf_aoi,
    gdf_adm0,
    gdf_adm1,
    gdf_adm0_surrounding= gdf_adm0_surrounding_simp,
    insivumeh_data_available,
    aoi_txt_label_size = 5,
    run_date
){
  
  run_mo <-  month(run_date)
  
  # At leadtime 0 INSIVUMEH is used
  run_mo_eq_lt0 <- run_mo %in% c(5,9)
  
  if(!insivumeh_data_available & !run_mo_eq_lt0){
    gdf_aoi <- gdf_aoi %>% 
      mutate(
        status = if_else(
          adm0_es == "Guatemala",
          as_factor("Not Available"),
          status)
        
      )
    
  }
  centroid_adm0 <- st_centroid(gdf_adm0)
  df_bbox <- st_bbox(gdf_adm0)
  pt_labels <- surrounding_country_labels()
  
  
  ggplot()+
    geom_sf(
      data= gdf_adm0_surrounding,
      color="black",
      fill=gghdx$hdx_hex("gray-light")
    ) + 
    geom_sf(
      data= gdf_adm0 ,
      fill = "white", color ="white"
    )+
    geom_sf(
      data= gdf_aoi,
      aes(fill = status),
      show.legend = c(fill=TRUE)
    )+
    scale_fill_manual(
      values = c(
        "Not Available" = gghdx$hdx_hex("gray-medium"),
        
        "No Activation"= "#55b284ff",#"#00ad78ff", # gghdx$hdx_hex("mint-ultra-light"),
        "Activation"=gghdx$hdx_hex("tomato-light")
        
      ),
      drop=FALSE
    )+
    geom_sf(
      data= gdf_adm1,
      color = gghdx$hdx_hex("gray-medium"),
      fill=NA, linewidth=0.2
    )+
    geom_sf(
      data= gdf_aoi,
      fill = NA, color = "darkgrey", linewidth = 1
      
    )+
    geom_sf(
      data= gdf_adm0 ,
      fill = NA, linewidth = 0.8
    )+
    

    geom_sf_text(
      data= centroid_adm0,
      aes(label= adm0_es), 
      color ="black", alpha=0.5,
      size=aoi_txt_label_size
    )+
    geom_sf_text(
      data= pt_labels,
      aes(label= adm0_es_label,angle=txt_angle), 
      color ="black", alpha=0.5,
      size=6
    )+
    coord_sf(xlim=df_bbox[c(1,3)],ylim=df_bbox[c(2,4)])+
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size=14),
      axis.line = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.background = element_rect(fill=gghdx$hdx_hex("sapphire-ultra-light"))
    )
}

#' 
#' @description
#' quick hack to make pt labels -- did with `mapedit::drawFeatures()` + `datapasta::tribble_paste()`
#' 
#' 
#' @return
#' @export
#'
#' @examples
surrounding_country_labels <- function(){
  df_pt <- tibble::tribble(
    ~adm0_es_label,         ~X,        ~Y, ~txt_angle,
    "Mexico", -91.45,  16.29313, 0,
    "Belize", -88.80, 16.75, 90,
    "Costa Rica", -84.8, 10.65,0
  )
  st_as_sf(x = df_pt, coords=c("X","Y"),crs=4326)
  
} 

#' @export
load_simplified_map_layers <- function(){
  l_fn <- list(
    AOI_ADM0 = "central_america_aoi_adm0_simplified.rds" ,
    AOI_SURROUNDING = "surrounding_aoi_adm0_simplified.rds",
    AOI_ADM1 = "central_america_aoi_adm1_simplified.rds"
  )
  purrr$map(
    l_fn,
    \(tfn){
      cumulus$blob_read(
        container ="projects",
        name = paste0("ds-aa-lac-dry-corridor/framework_update_2025/simple_map_layers/",tfn)
      )
    }
  )
}
