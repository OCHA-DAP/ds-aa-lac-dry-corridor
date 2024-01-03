
# subset raster

#' Title
#'
#' @param r 
#' @param gdf 
#' @param rm_dup_years 
#'
#' @return
#' @export
#'
#' @examples
zonal_gtm_nextgen <-  function(r, gdf,rm_dup_years=T){
  r_gtm <- unwrap(r)
  if(rm_dup_years){
    r_nm_yr_gtm <- as_date(str_extract(names(r_gtm),"\\d{4}-\\d{2}-\\d{2}") )
    r_gtm <- r_gtm[[year(r_nm_yr_gtm)>1982]] # these years have duplicate folders  
  }
  
  gdf_gtm_adm <- gdf %>% 
    map(
      ~.x %>% 
        filter(
          adm0_es=="Guatemala"
        )
    )
  exact_extract(x = r_gtm,
                y=gdf_gtm_adm$adm0,
                fun = "mean")%>% 
    pivot_longer(everything()) %>% 
    separate(name,into = c("stat","pub_date","lt_chr"),sep = "\\.") %>% 
    mutate(
      adm0_es ="Guatemala",
      pub_date= as_date(pub_date),
      lt = parse_number(lt_chr),
      valid_date = pub_date + months(lt)
    )
  
}



merge_gtm_nextgen <-  function(fps){
  # fps <- list.files(r_gdb,
  #                   full.names =T, 
  #                   recursive = T, 
  #                   pattern = "\\d{4}.nc$")
  # 
  
  lr <- fps %>% 
    map(\(fp_nc){
      
      # meta data collection    
      valid_time_meta <- ncmeta::nc_att(fp_nc,"T","units")$value$units
      start_time_meta <- ncmeta::nc_att(fp_nc,"S","units")$value$units
      valid_date <- as_date(str_extract(valid_time_meta,"\\d{4}-\\d{2}-\\d{2}"))
      start_date <- as_date(str_extract(start_time_meta,"\\d{4}-\\d{2}-\\d{2}"))
      lt <- month(valid_date)- month(start_date)
      bname<- paste0(start_date,".lt_",lt)
      cat(bname,"\n")
      
      dat <- ncdf4::nc_open(fp_nc)
      lon <- ncdf4::ncvar_get(dat,"X")
      lat <- ncdf4::ncvar_get(dat,"Y")
      value_array <- ncdf4::ncvar_get(dat,"deterministic")
      value_array_fixed <- aperm(value_array,c(2,1))
      
      rtmp <- terra::rast(
        x= value_array_fixed,
        extent =ext(
          min(lon),
          max(lon),
          min(lat),
          max(lat)
        ),
        crs= "OGC:CRS84"
      )
      rtmp %>% 
        set.names(bname)
      return(rtmp)
    }
    )
  r <- rast(lr )
  return(r)
  # terra::writeRaster(r, 
  #                    filename = "nextgen.tif",
  #                    filetype = "GTiff",
  #                    gdal=c("of=COG"))

  
  
}
write_gtm_nextgen <-  function(gdb, 
                               output_file_path
                               ){
  fps <- list.files(gdb,
                    full.names =T,
                    recursive = T,
                    pattern = "\\d{4}.nc$")


  
  lr <- fps %>% 
    map(\(fp_nc){
      
      # meta data collection  - to write band_name as "{pub_date}.lt_{leadtime}"
      valid_time_meta <- ncmeta::nc_att(fp_nc,"T","units")$value$units
      start_time_meta <- ncmeta::nc_att(fp_nc,"S","units")$value$units
      valid_date <- as_date(str_extract(valid_time_meta,"\\d{4}-\\d{2}-\\d{2}"))
      start_date <- as_date(str_extract(start_time_meta,"\\d{4}-\\d{2}-\\d{2}"))
      lead_time_diff <- interval(start_date,valid_date)
      lt <- lead_time_diff %/% months(1)
      bname<- paste0(start_date,".lt_",lt)
      
      # Now we open the each file and turn it into a terra::rast() 
      cat(bname,"\n")
      dat <- ncdf4::nc_open(fp_nc)
      lon <- ncdf4::ncvar_get(dat,"X")
      lat <- ncdf4::ncvar_get(dat,"Y")
      value_array <- ncdf4::ncvar_get(dat,"deterministic")
      value_array_fixed <- aperm(value_array,c(2,1))
      
      rtmp <- terra::rast(
        x= value_array_fixed,
        extent =ext(
          min(lon),
          max(lon),
          min(lat),
          max(lat)
        ),
        crs= "OGC:CRS84"
      )
      # set correct band nanme
      rtmp %>% 
        set.names(bname)
      return(rtmp)
    }
    )
  # merge multi-band
  r <- rast(lr )
  
  # write file to COG
  terra::writeRaster(r,
                     filename = output_file_path,
                     filetype = "GTiff",
                     gdal=c("of=COG"),overwrite=T)

  
  
}
catalogue_gtm_nextgen_files <-  function(gdb){
  fps <- list.files(gdb,
                    full.names =T,
                    recursive = T,
                    pattern = "\\d{4}.nc$")
  fps_w_parents <- list.files(gdb,
                         recursive = T,
                         pattern = "\\d{4}.nc$")
  lr <- fps %>% 
    map2(fps_w_parents,\(fp_nc,fp_par){
      
      fp_base <- basename(fp_nc)
      # meta data collection  - to write band_name as "{pub_date}.lt_{leadtime}"
      valid_time_meta <- ncmeta::nc_att(fp_nc,"T","units")$value$units
      start_time_meta <- ncmeta::nc_att(fp_nc,"S","units")$value$units
      valid_date <- as_date(str_extract(valid_time_meta,"\\d{4}-\\d{2}-\\d{2}"))
      start_date <- as_date(str_extract(start_time_meta,"\\d{4}-\\d{2}-\\d{2}"))
      lead_time_diff <- interval(start_date,valid_date)
      lt <- lead_time_diff %/% months(1)
      tibble(
        file_name_parent = fp_par,
        file_name= fp_base,
        pub_date = start_date,
        valid_date =valid_date,
        lt = lt
      )
    }) %>% 
    list_rbind()
}

merge_gtm_nextgen3 <-  function(fp){
      
      # meta data collection    
      valid_time_meta <- ncmeta::nc_att(fp,"T","units")$value$units
      start_time_meta <- ncmeta::nc_att(fp,"S","units")$value$units
      valid_date <- as_date(str_extract(valid_time_meta,"\\d{4}-\\d{2}-\\d{2}"))
      start_date <- as_date(str_extract(start_time_meta,"\\d{4}-\\d{2}-\\d{2}"))
      lt <- month(valid_date)- month(start_date)
      bname<- paste0(start_date,".lt_",lt)
      cat(bname,"\n")
      
      dat <- ncdf4::nc_open(fp)
      lon <- ncdf4::ncvar_get(dat,"X")
      lat <- ncdf4::ncvar_get(dat,"Y")
      value_array <- ncdf4::ncvar_get(dat,"deterministic")
      value_array_fixed <- aperm(value_array,c(2,1))
      
      r <- terra::rast(
        x= value_array_fixed,
        extent =ext(
          min(lon),
          max(lon),
          min(lat),
          max(lat)
        ),
        crs= "OGC:CRS84"
      )
      rtmp %>% 
        set.names(bname)
  
  terra::writeRaster(r,
                     filename = "nextgen.tif",
                     filetype = "GTiff",
                     gdal=c("of=COG"))

  
  
}

