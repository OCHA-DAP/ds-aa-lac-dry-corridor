
install.packages(c("dplyr", #wrangling
                   "ggplot2",# plotting
                   "readr", #reading
                   "purrr", # mapping
                   "tidyr", # pivot_wider
                   "lubridate",
                   "keyring",
                   "stringr", # annoying dep just for str_trim to add a single "." to email.
                   "zoo", # rolling stats
                   "janitor", # data cleaning - maybe not used?
                   "sf", # spatial 
                   "terra", # raster manipulation
                   "exactextractr", # zonal stats
                   "googledrive",
                   "here", # set paths in rmds without thinking.
                   "blastula", # rmd emails
                   "glue",  # mainly for pasting text in email.
                   "remotes", # needed to install gghdx
                   "showtext", # needed to set fonts in gghdx
                   "gghdx", # custom plotting
                   "rnaturalearth", # used to create AOI bbox
                   "ecmwfr"
                   ))

