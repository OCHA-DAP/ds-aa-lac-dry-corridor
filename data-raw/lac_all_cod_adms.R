#' This used to be part of the `_targets.R` pipeline, but I've decided to download the
#' files to the gdrive so that the pipeline runs off a static file that reflects the CODs at the
#' time of analysis. Updates to the CODs breaking this target is annoying because alot of the
#' heavier processes rely on it.

library(tidyverse)
library(sf)
library(rhdx)

# func in R folder that load the sf objectgs into alist
lgdf<- load_proj_admins() %>%
  map(~ .x %>%
        st_make_valid() %>%
        select(matches("^adm\\d_[ep]")))

lgdf %>%
  write_rds(
    file.path(
      Sys.getenv("AA_DATA_DIR"),
      "public",
      "processed",
      "lac",
      "lac_all_cod_adms.rds"
    )
  )
