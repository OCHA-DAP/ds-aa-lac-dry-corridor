merge_ecmwf_insuvimeh <- function(df_insuvimeh, df_ecmwf){
  
  df_ecmwf_insuv <- bind_rows(
    df_insuvimeh %>% 
      rename(
        mm= value
      ) %>% 
      mutate(
        source ="insuvimeh"
      ),
    df_ecmwf %>% 
      filter(adm0_es =="Guatemala") %>% 
      select(-value) %>% 
      mutate(
        source ="ecmwf"
      )
  )
  df_ecmwf_insuv %>% 
    filter(year(pub_date) %in% c(1983:2015)) %>% 
    mutate(
      decade =year(pub_date) - year(pub_date) %% 10,
      decade_abbr = paste0(decade,"'s")
    )
}