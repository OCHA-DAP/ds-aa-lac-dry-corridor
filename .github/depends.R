# install.packages("remotes") # moving this to the top to see if it will cause the GH action to fail faster


install.packages(c("dplyr",
                   "ggplot2",
                   "readr",
                   "sf",
                   # "rvest", # webscraping
                   "janitor", # data cleaning - used in scraper
                   "terra", # raster manipulation
                   "exactextractr", # zonal stats
                   "tidyr", # pivot_wider
                   # "zoo", # rolling stats
                   "googledrive",
                   "purrr",
                   "lubridate",
                   "stringr", # annoying dep just for str_trim to add a single "." to email.
                   "here", # set paths in rmds without thinking.
                   "blastula", # rmd emails
                   "glue",  # mainly for pasting text in email.
                   "remotes", # needed to install gghdx
                   "showtext", # needed to set fonts in gghdx
                   "gghdx"
                   ))

