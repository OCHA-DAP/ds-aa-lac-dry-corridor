#' iri_nc_to_r
#' 
#' @param tidync object created from iri `.nc` file with created with tidync::tidync(filepath) 
#' @param type \code{character} "dominant" or "prob"
#' @return list of terra raster objects
#'

iri_nc_to_r <-  function(
    tnc_object=iri_prob,
    type="dominant"
){
  
  # data array pub month
  da_pub_mon <- tnc_object %>% 
    activate("F") %>% 
    hyper_array()
  
  mo_count_start <- as_date("1960-01-01")
  
  mo_seq <- mo_count_start+months(floor(da_pub_mon$`F`))
  
  lon_lat_names <-  c("X","Y")
  da_coords <-  lon_lat_names%>% 
    map(
      ~ tnc_object   %>% 
        activate(.x) %>% 
        hyper_array()
    ) %>% 
    set_names(lon_lat_names)
  
  # hyper-frame split
  hf_split <- 
    c(
      lt1=1,
      lt2=2,
      lt3=3,
      lt4=4
    ) %>% 
    map(
      ~tnc_object%>% 
        hyper_filter(
          L= L==.x
        )
    )
  
  # extract data array from split hyper-frame
  da_split <- hf_split %>% 
    map(
      ~.x %>% 
        hyper_array(select_var = type)
    )
  
  if(type=="dominant"){
    
    # if dominant just loop through leadtimes
    r_split <- da_split %>%
      map(
        ~ rast(
          # aperm allows you to rerrange dimensions into those expected by terra::rast()
          x= aperm(.x[["dominant"]],c(2,1,3)),
          extent =ext(
            min(da_coords$X$X)-.5,
            max(da_coords$X$X)+.5,
            min(da_coords$Y$Y)-.5,
            max(da_coords$Y$Y)+.5
          ),
          crs= "EPSG:4326"
        ) 
      )
    r_split %>% 
      map(
        ~.x %>% 
          set.names(
            as.character(mo_seq)
          )
      )
    
  }
  
  if(type=="prob"){
    # if probability we have to first loop through leadtimes
    # and then loop through `C` which is tercile (below average, normal , above average)
    r_split <- da_split %>% 
      map(
        \(da){
          da_reordered <- aperm(da[["prob"]],c(2,1,4,3))
          r_tmp_split <- c(c1=1,c2=2,c3=3) %>% 
            map(\(c_prob){
              da_c<- da_reordered[,,,c_prob] 
              rtmp <- rast(
                x= da_c,
                extent =ext(
                  min(da_coords$X$X)-.5,
                  max(da_coords$X$X)+.5,
                  min(da_coords$Y$Y)-.5,
                  max(da_coords$Y$Y)+.5
                ),
                crs= "EPSG:4326")
              set.names(rtmp,mo_seq)
              return(rtmp)
            })
        }
      )}
  return(r_split)
}





