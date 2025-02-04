library(RNetCDF)
library(terra)
library(sf)
library(dplyr)
library(exactextractr)
library(lubridate)
source("R/tar_insuvimeh.R")

AOI_GTM <-  "Chiquimula"

gdf_adm1 <- cumulus::download_fieldmaps_sf(iso = "gtm",layer = "gtm_adm1")$gtm_adm1

gdf_gtm_aoi <- gdf_adm1 |> 
  filter(
    ADM1_ES == AOI_GTM
  )


gdb_insuvimeh_gtm <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private",
  "raw",
  "lac",
  "INSUVIMEH",
  "Pronósticos_Precip_NextGen_Guatemala"
)

r_insivumeh <- load_insuvimeh_raster2(gdb = gdb_insuvimeh_gtm)
r_insivumeh_unwraped <- unwrap(r_insivumeh)

df_insivumeh_wide <- exact_extract(
  x = r_insivumeh_unwraped,
  y = gdf_gtm_aoi,
  fun = "mean"
) 

df_insivumeh_long <- df_insivumeh_wide |> 
  pivot_longer(everything()) %>%
  separate(name, into = c("stat", "issued_date", "lt_chr"), sep = "\\.") %>%
  mutate(
    adm0_es = "Guatemala",
    adm1_es = AOI_GTM,
    issued_date = as_date(issued_date),
    leadtime = parse_number(lt_chr),
    valid_date = issued_date + months(leadtime)
  )


df_insivumeh_long_filtered <- df_insivumeh_long |> 
  filter(
    month(valid_date)%in% 5:11
  ) |> 
  filter(
    month(issued_date)!=2
  ) 


df_primera_insiv <- cumulus::seas5_aggregate_forecast(
    df_insivumeh_long_filtered,
    value ="value",
    valid_months = c(5:8),
    by = c("adm0_es","adm1_es","issued_date")
  )

df_postrera_insiv <- cumulus::seas5_aggregate_forecast(
    df_insivumeh_long_filtered,
    value ="value",
    valid_months = c(9:11),
    by = c("adm0_es","adm1_es","issued_date")
  )
ldf_insiv <- list(
  "primera"= df_primera_insiv,
  "postrera"= df_postrera_insiv
)


ldf_thresholded_1981_2022 <- ldf_insiv |> 
  map(
    \(dft){
      dft |> 
        filter(
          year(issued_date)<=2022
        ) |> 
        threshold_var(
          var= "value",
          by = c("adm0_es","adm1_es","leadtime"),
          rp_threshold = 4,
          direction =-1
        ) 
    }
  )

# now we can linearly interpolate:
rp_linear_funcs <- ldf_thresholded_1981_2022 |> 
  map(
    \(dft){
      dft |> 
        group_by(adm0_es,adm1_es,leadtime) |> 
        summarise(
          calc_empirical_rp_level = list(approxfun(rp_emp, value,method = "linear", rule =2,yright =Inf)),
          calc_empirical_rp = list(approxfun( value,rp_emp,method = "linear",rule =2))
        )
    }
  )


df_rp4_by_lt <- rp_linear_funcs |> 
  map(\(dft){
    dft |> 
      group_by(adm0_es,adm1_es, leadtime) |> 
      reframe(
        RP_empirical = 4,
        value_empirical = map_dbl(RP_empirical,calc_empirical_rp_level),
        # these values are exactly the same as already calculated in `df_lp3_rps` so this
        # step is redundant, but will pack it in here anyways
        # value_LP3 = map_dbl(RP_empirical,calc_lp3_rp_level),
        # RP_LP3_calc = map_dbl(value_empirical,calc_lp3_rp)
      )
  }
  )


ldf_seas5_thresholded_w_recent <-   map(
  set_names(names(df_rp4_by_lt),names(df_rp4_by_lt)),
  \(season_temp){
    
    df_thresh <- df_rp4_by_lt[[season_temp]]
    df_historical  = ldf_insiv[[season_temp]]
    df_historical |> 
      left_join(df_thresh) |> 
      mutate(
        mm_flag = value<=value_empirical
      )
  })

p_timeseries_refined_aoi_1988_2022_base <- ldf_seas5_thresholded_w_recent |> 
  imap(
    \(dft,nmt){
      month_range_temp <- ifelse(nmt == "primera","MJJA","SON")
      dft_min <- dft |> 
        # get min per issued date (across LTs)
        group_by(adm0_es,adm1_es,year(issued_date)) |> 
        slice_min(
          order_by = value, n= 1
        ) 
      dft_min |> 
        ggplot(
          aes(x= year(issued_date),y=value, group = 1)
        )+
        geom_point(aes(color = mm_flag), alpha= 0.7, size= 4)+
        geom_line(color = "black")+
        # facet_wrap(~iso3)+
        scale_color_manual(values = c(hdx_hex("sapphire-hdx"),hdx_hex("tomato-hdx")))+
        labs(
          title = glue("Guatemala - INSIVUMEH - {month_range_temp}-Forecasts"),
          subtitle = glue("{str_to_title(nmt)}:{AOI_GTM}"),
          caption = "Plotting minimum rainfall at any leadtime monitoried in AA Framework
          Analysis performed at sub-national levelbased on area-weighted average of admins of interest per country"
        ) +
        geom_text_repel(
          data= filter(dft_min,mm_flag),
          aes(label = year(issued_date)), color = hdx_hex("tomato-hdx")
        )+
        theme(
          axis.title = element_blank(),
          legend.position = "none"
        )
    }
  )

p_timeseries_refined_aoi_1988_2022_base$primera
p_timeseries_refined_aoi_1988_2022_base$postrera

