#' grib_meta_table
#'
#' @param path `character` path to grib file
#'
#' @return data.frame containing key metadata to properly filter and label gribs after loading with `terra::rast()`
#' @examples \dontrun{
#' fp_gribs <- list.files(
#'             file.path("ecmwf_gribs"),
#'             pattern = "\\.grib$",full.names = T
#'                        )
#' grib_meta_table(path=fp_gribs[1])
#' }
grib_meta_table <- function(path){
  # command line
  grib_meta <- system(paste("grib_ls", path), intern = TRUE)
  remove <- c(1, (length(grib_meta)-2):length(grib_meta)) 
  grib_meta <- grib_meta[-remove]
  
  # Processing metadata to join with actual data
  meta <- read.table(text = grib_meta, header = TRUE) |>
    as_tibble() |> 
    janitor::clean_names() |> 
    mutate(
      variable_id = as.character(glue::glue("{data_date}_step{step_range}_{data_type}_{short_name}")),
      pub_date = ymd(data_date),
      lt = round(step_range/(24*30),0),
      uid =row_number()
      
    )  |> 
    select(-grid_type, -packing_type, -level, -type_of_level, -centre, -edition)
  return(meta)
}




#' Title
#'
#' @param file_paths 
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' 
#' fp_gribs <- list.files(
#' file.path("ecmwf_gribs"),
#' pattern = "\\.grib$",full.names = T
#' )
#' fp_gribs <- str_subset(string = fp_gribs,
#'                        pattern = "2024-02")
#'                        }
#'                        
load_mf_ensemble_mean <-  function(file_paths){
  file_paths %>% 
    map(\(path_tmp){
      r_tmp <-  rast(path_tmp)
      meta_table <- grib_meta_table(path_tmp) 
      idx <- which(meta_table$data_type=="em")
      r_subset <- r_tmp[[idx]]
      pub_date <-  unique(meta_table$pub_date)
      lt_adj <- unique(meta_table$lt)-1
      valid_date <- pub_date + months(lt_adj)
      
      band_label <-paste0(pub_date,".",lt_adj)
      time(r_subset) <- valid_date
      days_in_month(time(r_subset))
      
      
      r_subset = r_subset * days_in_month(time(r_subset)) * 24 * 3600 * 1000
      names(r_subset) <- band_label
      # convert tprate to mm/month -- use something similar to python trick.
      return(r_subset)
    }
    )
}


load_mf_ensembles <-  function(file_paths){
  file_paths %>% 
    map(\(path_tmp){
      # path_tmp <-  fp_gribs[1]
      r_tmp <-  rast(path_tmp)
      meta_table <- grib_meta_table(path_tmp) 
      idx <- which(meta_table$data_type=="fcmean")
      r_subset <- r_tmp[[idx]]
      r_mean <- mean(r_subset)
      pub_date <-  unique(meta_table$pub_date)
      lt_adj <- unique(meta_table$lt)-1
      valid_date <- pub_date + months(lt_adj)
      
      band_label <-paste0(pub_date,".",lt_adj,".",1:length(unique(idx)))
      time(r_subset) <- rep(valid_date,nlyr(r_subset))
      
      
      r_subset = r_subset * days_in_month(valid_date) * 24 * 3600 * 1000
      names(r_subset) <- band_label
      # convert tprate to mm/month -- use something similar to python trick.
      return(r_subset)
    }
    )
  
}

