
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

