library(rhdx)
library(janitor)

load_proj_admins <- function() {
  # Nicaragua
  lyr_name_nic_adm <- c(
    adm0 = "nic_admbnda_adm0_inide_itos_20210826",
    adm1 = "nic_admbnda_adm1_inide_itos_20210826",
    adm2 = "nic_admbnda_adm2_inide_20210826"
  )

  gdfl_nic_adm <- lyr_name_nic_adm %>%
    map(
      ~ search_datasets("Nicaragua - Subnational Administrative Boundaries") %>%
        pluck(1) %>%
        get_resource(2) %>%
        read_resource(layer = .x) %>%
        clean_names() %>%
        select(matches("^adm\\d_"))
    )

  # Guatemala
  # 	OCHA Field Information Services Section (FISS)
  # https://data.humdata.org/dataset/cod-ab-gtm
  # all the others have columns "adm\\d_es" and "adm\\d_pcode" - so change this one to match

  lyr_name_gtm_adm <- c(
    adm0 = "gtm_admbnda_adm0_ocha_conred_20190207",
    adm1 = "gtm_admbnda_adm1_ocha_conred_20190207",
    adm2 = "gtm_admbnda_adm2_ocha_conred_20190207"
  )
  gdfl_gtm_adm <- lyr_name_gtm_adm %>%
    map(
      ~ search_datasets("Guatemala - Subnational Administrative Boundaries") %>%
        pluck(1) %>%
        get_resource(2) %>%
        read_resource(layer = .x) %>%
        clean_names() %>%
        rename_with(
          .cols = everything(),
          ~ str_replace_all(.x, "admin", "adm") %>%
            str_replace_all("name[*_]", "_") %>%
            str_replace_all("pcode", "_pcode")
        ) %>%
        # luckily found this: https://github.com/r-spatial/sf/issues/1509
        st_sf() %>%
        rename(
          geometry = "Shape"
        )
    )



  # El Salvador
  # https://data.humdata.org/dataset/cod-ab-slv
  lyr_name_slv_adm <- c(
    adm0 = "slv_admbnda_adm0_gadm_20210204",
    adm1 = "slv_admbnda_adm1_gadm_20210204",
    adm2 = "slv_admbnda_adm2_gadm_20210204"
  )

  gdfl_slv_adm <- lyr_name_slv_adm %>%
    map(
      ~ search_datasets("cod-ab-slv") %>%
        pluck(1) %>%
        get_resource(2) %>%
        read_resource(layer = .x) %>%
        clean_names() %>%
        select(matches("^adm\\d_"))
    )

  # Honduras
  # there is something very wrong w/ the Honduras files on HDX
  # https://data.humdata.org/dataset/cod-ab-hnd
  # had to download and procdess them in QGIS as geopackage until they were readable
  # can't read from `{rhdx}` either.
  fp_hnd_cod <- file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public",
    "raw",
    "hnd",
    "hnd_ab",
    "hnd_cod.gpkg"
  )

  lyr_nm <- st_layers(fp_hnd_cod)
  gdfl_hnd_adm <- lyr_nm$name %>%
    set_names("adm0", "adm1", "adm2") %>%
    map(
      ~ st_read(dsn = fp_hnd_cod, layer = .x) %>%
        clean_names() %>%
        select(matches("^adm\\d_*")) %>%
        rename(geometry = "geom")
    )


  c("adm0", "adm1", "adm2") %>%
    map(
      ~ list(
        slv = gdfl_slv_adm,
        gtm = gdfl_gtm_adm,
        hnd = gdfl_hnd_adm,
        nic = gdfl_nic_adm
      ) %>%
        map_dfr(.x)
    ) %>%
    set_names(c("adm0", "adm1", "adm2"))
}
