
load_ncdf_blob_insiv <- function(
    run_date = Sys.Date(),
    container = "global",
    dir = "raster/raw",
    stage = "dev"
){
  run_mo = lubridate::month(run_date,label = TRUE, abbr = TRUE)
  run_yr = lubridate::year(run_date)
  
  prefix<- paste0("insivumeh_pronos_deterministic")
  suffix<- paste0("start",run_mo,run_yr,".nc")
  rgx <- glue::glue("^{dir}/{prefix}.*{suffix}$")
  
  container_client <- cumulus::azure_container(
    container = container,
    stage = stage
    )

  blob_contents <- AzureStor::list_blobs(container = container_client,dir = "raster/raw")
  blob_names <- stringr::str_subset(blob_contents$name,pattern = rgx)
  
  td <- tempdir()
 
  blob_names |> 
    purrr::map(
      \(b){
        
        tf <- file.path(td,basename(b))
        
        AzureStor::download_blob(
          container = container_client,
          src = b,
          dest = tf
        )
        
      }
    )
  r <- load_ncdf_insivumeh(gdb = td)
  r
}



#' @export
load_proj_contatiners <- function() {
  es <- azure_endpoint_url()
  # storage endpoint
  se <- AzureStor::storage_endpoint(es, sas = Sys.getenv("DSCI_AZ_SAS_DEV"))
  # storage container
  sc_global <- AzureStor::storage_container(se, "global")
  sc_projects <- AzureStor::storage_container(se, "projects")
  list(
    GLOBAL_CONT = sc_global,
    PROJECTS_CONT = sc_projects
  )
}

#' @export
azure_endpoint_url <- function(
    service = c("blob", "file"),
    stage = c("dev", "prod"),
    storage_account = "imb0chd0") {
  blob_url <- "https://{storage_account}{stage}.{service}.core.windows.net/"
  service <- rlang::arg_match(service)
  stage <- rlang::arg_match(stage)
  storae_account <- rlang::arg_match(storage_account)
  endpoint <- glue::glue(blob_url)
  return(endpoint)
}

check_insivumeh_blob <- function(run_date){
  DIR_CURRENT_INSIV <- paste0("start",month(run_date,abbr = T,label = T))
  blob_name_rgx <- paste0("start",format(run_date,"%b%Y"),".nc$")
  pc <- load_proj_contatiners()
  
  cont_contents <- AzureStor::list_blobs(
    pc$GLOBAL_CONT, dir = "raster/raw"
  )
  fps_blob <- str_subset(cont_contents$name,blob_name_rgx)
  cat("checking global/raster/raw for 6 new forecast files\n")
  filenames_unique <-  unique(fps_blob)
  num_unique_files <- length(fps_blob)
  
  ret_lgl <- num_unique_files==6
  if(ret_lgl){
    cat("6 unique INSIVUMEH files found for current run month")
  }else{
    cat("6 unique INSIVUMEH files NOT FOUND for current month")
  }
  ret_lgl
}
