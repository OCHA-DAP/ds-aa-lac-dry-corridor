
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

# basename(fps)
# fps <- tail(fps,10)
# fp_nc<- fps[length(fps)]
# tidync::tidync(fpt)
# ncmeta::nc_att(fpt,"T","units")$value$units
# ncmeta::nc_att(fpt,"S","units")$value$units
# lr %>% 
#   map(
#     ~ext(.)
#   )
# plot(lr[[4]])
# plot(lr[[5]])
# tar_load(lgdf)
# tar_load(gdf_aoi_adm)
# gdf_bbox <- gdf_aoi_adm$adm0 %>% 
#   filter(adm0_es=="Guatemala") %>% 
#   st_bbox() %>% 
#   st_as_sfc()
# 
# recent <- crop(lr[[10]],gdf_bbox)
# old <- crop(lr[[3]],gdf_bbox)
# ext(recent)==ext(old)
# ext(lr[[10]])
# ext(lr[[1]])
# extent_old <- ext(old)




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
  return(lr)
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




# tiffs - NRT -------------------------------------------------------------



#' Title
#'
#' @param r 
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' r_stack<- rast(list.files(file.path(gdb_insuvimeh_nrt_manual,"startJan"),full.names = T)) 
#' insuvimeh_nrt_lookup(r_stack)
#' }
insuvimeh_nrt_lookup <- function(r){
  r_names <- names(r)
  mo_rgx <- glue_collapse(month(1:12,abbr=T,label = T),"|")
  
  valid_pub_mos <- str_extract_all(r_names,mo_rgx) 
  
  
  
  df_lookup <- tibble(
    r_names,
    mo_pair = valid_pub_mos
  ) 
  
  df_lookup %>% 
    unnest_wider(col = mo_pair,names_sep=",") %>% 
    rename(
      pub_mo = 3,
      valid_mo=2
    ) %>% 
    mutate(
      pub_yr = str_extract(r_names, "\\d{4}"),
      pub_mo_int = match(pub_mo,month(1:12, label=T, abbr=T)),
      valid_mo_int = match(valid_mo,month(1:12, label=T, abbr=T)),
      pub_date = ym(paste0(pub_yr,"-", formatC(pub_mo_int,flag = "0",width = 2))),
      valid_date = ym(paste0(pub_yr,"-", formatC(valid_mo_int,flag = "0",width = 2))),
      lt =interval(pub_date, valid_date) %/% months(1),
      band_label = paste0(pub_date, ".lt_",lt)
      
      
      
    )
}
#' Title
#'
#' @param r 
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' r_stack<- rast(list.files(file.path(gdb_insuvimeh_nrt_manual,"startJan"),full.names = T)) 
#' rename_insuvimeh_nrt_bands(r_stack)
#' }
rename_insuvimeh_nrt_bands <-  function(r){
  df_lookup <- insuvimeh_nrt_lookup(r=r)
  set.names(r,df_lookup$band_label)
  return(r)
}

#' Title
#'
#' @param r 
#' @param gdf 
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' r_stack<- rast(list.files(file.path(gdb_insuvimeh_nrt_manual,"startJan"),full.names = T)) 
#' tar_load(gdf_aoi_adm)
#' zonal_insuvimeh_nrt(r_stack,  gdf =gdf_aoi_adm )
#' }}
zonal_insuvimeh_nrt <-  function(r, gdf){
  r <- rename_insuvimeh_nrt_bands(r)
  # old_names <- names(r)
  # new_names <-names(r)
  # tibble(old_names,new_names)
  
  gdf_gtm_adm <- gdf %>% 
    map(
      ~.x %>% 
        filter(
          adm0_es=="Guatemala"
        )
    )
  
  exact_extract(x = r,
                y=gdf_gtm_adm$adm0,
                fun = "mean")%>% 
    pivot_longer(everything()) %>% 
    separate(name,into = c("stat","pub_date","lt_chr"),sep = "\\.") %>% 
    mutate(
      adm0_es ="Guatemala",
      pub_date= as_date(pub_date),
      lt = parse_number(lt_chr),
      valid_date = pub_date + months(lt),
      window= case_when(
        month(valid_date) %in% c(5,6,7,8)~"Primera",
        month(valid_date) %in% c(9,10,11)~"Postrera",
        .default = NA
      )
    )
}

# summarise_insuvimeh_to_season <-  function()

