# library(girafe)
# library(tidyverse)

#' get_formated_bucket_df
#' get data.frame cataloguing bucket objects

get_formatted_bucket_df <-  function(bucket=Sys.getenv("BUCKET_NAME")){
  aws.s3::get_bucket_df(bucket,max = 10000) %>%
    dplyr::as_tibble() %>% 
    dplyr::mutate(
      date= lubridate::as_date(LastModified)
    ) %>% 
    # mutate(
    #   uid_time= str_remove(LastModified,"\\.000Z"),
    #   size_mb = as.numeric(Size) * 1e-6
    # ) %>% 
    tidyr::separate(LastModified,into = c("date","time"), sep = "T") %>% 
    dplyr::mutate(
      time= stringr::str_remove(time,"\\.000Z"),
      time = stringr::str_replace_all(time,":","-"),
      filename = paste0(date,"_",time,".grib"),
      date= lubridate::as_date(date),
      size_mb = as.numeric(Size) * 1e-6
    )
}

# viz_bucket_objects(bucket =Sys.getenv("BUCKET_NAME"),
#                    interactive_plot = T )
viz_bucket_objects <- function(
    bucket=Sys.getenv("BUCKET_NAME"),
    interactive_plot=T
){
  bucket_df <- get_formatted_bucket_df(bucket= bucket)
  bucket_df %>% 
    arrange(desc(size_mb))
  p <- bucket_df %>% 
    ggplot(
      aes(x= date, y= size_mb)
    )
  if(interactive_plot){
    p <- p + 
      geom_jitter_interactive(
        aes(tooltip = glue("Object: {filename}
                           Size: {round(size_mb,1)} mb"))
      )+
      labs(x= "modified date", y= "size (mb)")
  }
  if(!interactive_plot){
    p <- p +
      geom_jitter()+
      labs(x= "modified date", y= "size (mb)")
  }
  girafe(ggobj = p)
}