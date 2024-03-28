



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
    gdf_adm0,
    gdf_adm1,
    gdf_adm0_surrounding= gdf_adm0_surrounding_simp,
    aoi_txt_label_size = 5
    ){
  centroid_adm0 <- st_centroid(gdf_adm0)
  df_bbox <- st_bbox(gdf_adm0)
  pt_labels <- surrounding_country_labels()
  
  ggplot()+
    geom_sf(
      data= gdf_adm0,
      aes(fill = status)
    )+
    scale_fill_manual(
      values = c("Activation"=hdx_hex("tomato-light"),
                 "No Activation"=hdx_hex("mint-ultra-light")),
      drop=FALSE
    )+
    
    geom_sf(
      data= gdf_adm0_surrounding,
      color="black",
      fill="white"
    )+
    geom_sf(
      data= gdf_adm1,
      color = hdx_hex("gray-medium"),
      fill=NA,
      alpha= 1
    )+
    geom_sf(
      data= gdf_adm0,
      color = "black",
      fill=NA
    )+
    geom_sf_text(
      data= centroid_adm0,
      aes(label= adm0_es), 
      color ="black",
      size=aoi_txt_label_size
    )+
    geom_sf_text(
      data= pt_labels,
      aes(label= adm0_es_label,angle=txt_angle), 
      color ="grey",
      size=6
    )+
    coord_sf(xlim=df_bbox[c(1,3)],ylim=df_bbox[c(2,4)])+
    theme(
      legend.title = element_blank(),
      axis.line = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.background = element_rect(fill=hdx_hex("sapphire-ultra-light"))
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

