#' In process of revamping framework for 2025 needed to make some adjustments to file storage
#' during the 2024 framework DEVELOPMENT I had originally stored INSIVUMEH data on old Gdrive
#' during the 2024 framework MONITORING I had used a mix and mostly stored new incoming data on the blob.
#' 
#' therefore I want to get all the data in one good location before moving to the blob eventually.
#' Therefore I've done the following:
#' 1. Download any data that was from blob that was never put on GDRIVE to the old GDRIVE directory
#' 2. Here I copy the data from the old GDRIVE to the new GDRIVE. I'm actually doing this as a script
#' because I get an error when I try to copy so many files at once manualy w/ drag & drop. Therefore we just loop here
#' 3. I did manually copy the `new_format` 2024 data from old GDRIVE to new GDRIVe


library(tidyverse)

fp_old <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "raw",
  "lac",
  "INSUVIMEH",
  "Pronósticos_Precip_NextGen_Guatemala"
)


fp_new <- file.path(
  Sys.getenv("AA_DATA_DIR_NEW"),
  "private",
  "raw",
  "lac",
  "INSIVUMEH",
  "old_format"
  )

l_tmp_dir <- list.dirs(fp_old, recursive = F)
l_tmp_dir |> 
  map(\(tmp_dir_old){
    # tmp_dir_old <- l_tmp_dir[2]
    bname_dir <- basename(tmp_dir_old)
    cat(bname_dir,"\n")
    tmp_dir_new <- file.path(
      fp_new,
      bname_dir
    )
    dir.create(
      tmp_dir_new
    )
    file.copy(
      list.files(tmp_dir_old,full.names = T),
      tmp_dir_new,
      recursive = T
    )
    
  })

