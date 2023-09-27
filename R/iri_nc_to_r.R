#' iri_nc_to_r
#' @description
#' Background: we have a python to download and pre-process IRI seasonal forecast data as `.nc` files.
#' Once this is been complete (iri.download()->iri.process()) we want to do further manipulation/processing in R.
#' You can read the `.nc` objet in with `{ncdf4}`, `{tidync}`, and other packages. This function assumes use of `{tidync}` and 
#' takes a "tidync" class object as the main input
#' 
#' Objective:
#' Transform tidync IRI forecast (probability tercile bands, or dominant tercile bands) and convert to terra "raster" class for
#' easier subsequent processing
#' 
#' Note:
#' This is currently a custom process for IRI data and this particular project
#' 
#' @param tidync "tidync" class object created from iri `.nc` file with created with tidync::tidync(filepath) 
#' @param type \code{character} "dominant" or "prob"
#' @return list of terra raster objects.
#'   if type = "dominant" it will return 1 terra object with 4 leadtime bands
#'   if type = "prob" it will return a list of 3 terra raster objects (each with 4 leadtime bands):
#'    1. probability below average,
#'    2. probability normal
#'    3. probability above average



iri_nc_to_r <-  function(
    tnc_object=iri_prob,
    type="dominant"
){
  
  # data array pub month
  da_pub_mon <- tnc_object %>% 
    activate("F") %>% 
    hyper_array()
  
  # IRI months are the number of months since Jan 1960 - convert sequence to dates.
  # When we later convert to `{terra}` we get a multiylayer raster where each layer represents values from
  # one particular date - since layers are not well labelled (by default) we  use this vector to properly label layers
  
  mo_seq <- as_date("1960-01-01") + months(floor(da_pub_mon$`F`))
  
  da_coords <-  map(
      set_names(c("X","Y")),
      ~ tnc_object   %>% 
        activate(.x) %>% 
        hyper_array()
    ) 
  
  # hyper-frame split
  # split each hyper-frame by leadtime (`L`) so that we can process and convert each leadtime into
  # it's own band
  
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
   
  da_split <-  c(
     lt1=1,
     lt2=2,
     lt3=3,
     lt4=4
   ) %>% 
     map(
       ~tnc_object%>% 
         hyper_filter(
           L= L==.x
         ) %>% 
         hyper_array()
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





