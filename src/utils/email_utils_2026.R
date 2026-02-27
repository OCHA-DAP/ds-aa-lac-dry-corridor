#' Email utilities for 2026 CADC drought monitoring
#'
#' Bilingual (EN/ES) email text builders, gt table builders, and send helper.
#' Imports shared utilities (add_ggplot_custom, load_email_recipients) from email_utils.

box::use(
  dplyr[...],
  glue[...],
  stringr,
  gt,
  sf,
  purrr,
  blastula[...],
  logger
)

# Constants ---------------------------------------------------------------

CHD_GREEN <- "#55b284ff"
CHD_DARK_GREEN <- "#3e8f6b"
CHD_TOMATO <- "#F2645A"

OCHA_RP_FOOTNOTE_EN <- "Thresholds calculated from ECMWF SEAS5 hindcasts (1991-2024) to approximate a ~3 year return period drought level for seasonal rainfall."
OCHA_RP_FOOTNOTE_ES <- "Umbrales calculados a partir de los hindcasts de ECMWF SEAS5 (1991-2024) para aproximar un nivel de sequ\u00eda con un per\u00edodo de retorno de ~3 a\u00f1os para la precipitaci\u00f3n estacional."

SN_RP_FOOTNOTE_EN <- "Thresholds calculated from ECMWF SEAS5 hindcasts (1991-2024) to approximate a ~4.4 year return period drought level."
SN_RP_FOOTNOTE_ES <- "Umbrales calculados a partir de los hindcasts de ECMWF SEAS5 (1991-2024) para aproximar un nivel de sequ\u00eda con un per\u00edodo de retorno de ~4.4 a\u00f1os."


# Internal helpers --------------------------------------------------------

MONTH_NAMES_ES <- c(
  "enero", "febrero", "marzo", "abril", "mayo", "junio",
  "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"
)

month_name_es <- function(date) {
  stringr$str_to_title(MONTH_NAMES_ES[as.integer(format(date, "%m"))])
}

status_html <- function(activated, lang = "en") {
  if (activated) {
    label <- if (lang == "es") "Activado" else "Activated"
    glue("<span style='color: {CHD_TOMATO};'>{label}</span>")
  } else {
    label <- if (lang == "es") "No Activado" else "Not Activated"
    glue("<span style='color: {CHD_GREEN};'>{label}</span>")
  }
}

monitored_range_label <- function(season, lang = "en") {
  if (lang == "es") {
    ifelse(season == "Primera", "mayo-agosto", "septiembre-noviembre")
  } else {
    ifelse(season == "Primera", "May-August", "September-November")
  }
}


# OCHA email text ---------------------------------------------------------

#' Build bilingual OCHA email text
#'
#' @param df_status data.frame with status_lgl, adm0_es columns (OCHA countries)
#' @param run_date Date of the monitoring run
#' @param season character "Primera" or "Postrera"
#' @param monitored_range character e.g. "May-August"
#' @return list with $subj, $en, $es sub-lists
#' @export
build_email_text_ocha <- function(df_status, run_date, season, monitored_range) {
  month_en <- as.character(format(run_date, "%B"))
  month_es <- month_name_es(run_date)
  date_fmt <- trimws(format(run_date, "%e %B %Y"))
  date_fmt_es <- glue("{trimws(format(run_date, '%e'))} de {tolower(month_es)} de {format(run_date, '%Y')}")
  monitored_es <- monitored_range_label(season, "es")

  df_activated <- df_status |> filter(status_lgl)
  activated <- nrow(df_activated) > 0

  subj <- glue(
    "AA Central America Dry Corridor - Drought Monitoring - {month_en} update - ",
    "{ifelse(activated, 'Activated', 'No Activations')} (HND, SLV, GTM)"
  )

  # English
  if (activated) {
    countries <- glue_collapse(unique(df_activated$adm0_es), sep = ", ", last = " & ")
    desc_en <- glue(
      "The AA framework has been triggered in {countries} where the combined ",
      "rainfall forecast over the 2026 {season} season ({monitored_range}) is predicted ",
      "to be below drought trigger levels. The trigger status and thresholds are based on ",
      "the latest ECMWF SEAS5 forecast and historical ECMWF SEAS5 hindcasts (1991-2024) ",
      "for each country independently."
    )
    desc_es <- glue(
      "El marco de Acci\u00f3n Anticipatoria se ha activado en {countries} donde el ",
      "pron\u00f3stico de precipitaci\u00f3n acumulada para la temporada {season} 2026 ",
      "({monitored_es}) se prev\u00e9 que est\u00e9 por debajo de los niveles de activaci\u00f3n ",
      "por sequ\u00eda. El estado de activaci\u00f3n y los umbrales se basan en el \u00faltimo ",
      "pron\u00f3stico ECMWF SEAS5 y los hindcasts hist\u00f3ricos de ECMWF SEAS5 (1991-2024) ",
      "para cada pa\u00eds de forma independiente."
    )
  } else {
    desc_en <- glue(
      "The AA framework has not triggered in any country. The total rainfall forecast ",
      "over the 2026 {season} season ({monitored_range}) is not predicted to be below ",
      "drought trigger levels. The trigger status and thresholds are based on the latest ",
      "ECMWF SEAS5 forecast and historical ECMWF SEAS5 hindcasts (1991-2024) for each ",
      "country independently."
    )
    desc_es <- glue(
      "El marco de Acci\u00f3n Anticipatoria no se ha activado en ning\u00fan pa\u00eds. ",
      "El pron\u00f3stico de precipitaci\u00f3n total para la temporada {season} 2026 ",
      "({monitored_es}) no se prev\u00e9 que est\u00e9 por debajo de los niveles de ",
      "activaci\u00f3n por sequ\u00eda. El estado de activaci\u00f3n y los umbrales se basan ",
      "en el \u00faltimo pron\u00f3stico ECMWF SEAS5 y los hindcasts hist\u00f3ricos de ",
      "ECMWF SEAS5 (1991-2024) para cada pa\u00eds de forma independiente."
    )
  }

  list(
    subj = subj,
    en = list(
      title = "Anticipatory Action - Central American Dry Corridor",
      subtitle = glue("2026 {season} Drought Monitoring - {month_en} Update"),
      date_header = glue("{date_fmt} - Trigger status:"),
      status = status_html(activated, "en"),
      description_title = "Trigger Description",
      description_content = desc_en,
      contact_info = "Contact the OCHA Centre for Humanitarian Data via Leonardo Milano, Team Lead\nfor Data Science at leonardo.milano@un.org with any questions or feedback.",
      data_source = "ECMWF SEAS5",
      ref_github = "Full documentation and source code can be found in the [GitHub repository](https://github.com/OCHA-DAP/ds-aa-lac-dry-corridor).",
      tbl_footnote = OCHA_RP_FOOTNOTE_EN
    ),
    es = list(
      title = "Acci\u00f3n Anticipatoria - Corredor Seco Centroamericano",
      subtitle = glue("Monitoreo de Sequ\u00eda {season} 2026 - Actualizaci\u00f3n de {month_es}"),
      date_header = glue("{date_fmt_es} - Estado de activaci\u00f3n:"),
      status = status_html(activated, "es"),
      description_title = "Descripci\u00f3n del Mecanismo de Activaci\u00f3n",
      description_content = desc_es,
      contact_info = "Contacte al Centro de Datos Humanitarios de OCHA a trav\u00e9s de Leonardo Milano, Team Lead\nfor Data Science en leonardo.milano@un.org para cualquier pregunta o comentario.",
      data_source = "ECMWF SEAS5",
      ref_github = "La documentaci\u00f3n completa y el c\u00f3digo fuente se encuentran en el [repositorio de GitHub](https://github.com/OCHA-DAP/ds-aa-lac-dry-corridor).",
      tbl_footnote = OCHA_RP_FOOTNOTE_ES
    )
  )
}


# StartNetwork email text -------------------------------------------------

#' Build bilingual StartNetwork email text
#'
#' @param df_status data.frame with status_lgl column (StartNetwork)
#' @param run_date Date of the monitoring run
#' @param season character "Primera" or "Postrera"
#' @param monitored_range character e.g. "May-August"
#' @return list with $subj, $en, $es sub-lists
#' @export
build_email_text_sn <- function(df_status, run_date, season, monitored_range) {
  month_en <- as.character(format(run_date, "%B"))
  month_es <- month_name_es(run_date)
  date_fmt <- trimws(format(run_date, "%e %B %Y"))
  date_fmt_es <- glue("{trimws(format(run_date, '%e'))} de {tolower(month_es)} de {format(run_date, '%Y')}")
  monitored_es <- monitored_range_label(season, "es")

  df_activated <- df_status |> filter(status_lgl)
  activated <- nrow(df_activated) > 0

  subj <- glue(
    "AA StartNetwork Guatemala - Drought Monitoring - {month_en} update - ",
    "{ifelse(activated, 'Activated', 'No Activations')}"
  )

  if (activated) {
    desc_en <- glue(
      "The StartNetwork AA framework has been triggered. The combined rainfall forecast ",
      "over the 2026 {season} season ({monitored_range}) for the Quich\u00e9/Baja Verapaz ",
      "AOI is predicted to be below drought trigger levels. The trigger status and thresholds ",
      "are based on the latest ECMWF SEAS5 forecast and historical ECMWF SEAS5 hindcasts (1991-2024)."
    )
    desc_es <- glue(
      "El marco de Acci\u00f3n Anticipatoria de StartNetwork se ha activado. El pron\u00f3stico ",
      "de precipitaci\u00f3n acumulada para la temporada {season} 2026 ({monitored_es}) para el ",
      "\u00e1rea de inter\u00e9s de Quich\u00e9/Baja Verapaz se prev\u00e9 que est\u00e9 por debajo ",
      "de los niveles de activaci\u00f3n por sequ\u00eda. El estado de activaci\u00f3n y los umbrales ",
      "se basan en el \u00faltimo pron\u00f3stico ECMWF SEAS5 y los hindcasts hist\u00f3ricos de ",
      "ECMWF SEAS5 (1991-2024)."
    )
  } else {
    desc_en <- glue(
      "The StartNetwork AA framework has not triggered. The total rainfall forecast ",
      "over the 2026 {season} season ({monitored_range}) for the Quich\u00e9/Baja Verapaz ",
      "AOI is not predicted to be below drought trigger levels. The trigger status and thresholds ",
      "are based on the latest ECMWF SEAS5 forecast and historical ECMWF SEAS5 hindcasts (1991-2024)."
    )
    desc_es <- glue(
      "El marco de Acci\u00f3n Anticipatoria de StartNetwork no se ha activado. El pron\u00f3stico ",
      "de precipitaci\u00f3n total para la temporada {season} 2026 ({monitored_es}) para el ",
      "\u00e1rea de inter\u00e9s de Quich\u00e9/Baja Verapaz no se prev\u00e9 que est\u00e9 por debajo ",
      "de los niveles de activaci\u00f3n por sequ\u00eda. El estado de activaci\u00f3n y los umbrales ",
      "se basan en el \u00faltimo pron\u00f3stico ECMWF SEAS5 y los hindcasts hist\u00f3ricos de ",
      "ECMWF SEAS5 (1991-2024)."
    )
  }

  list(
    subj = subj,
    en = list(
      title = "Anticipatory Action - StartNetwork Guatemala",
      subtitle = glue("2026 {season} Drought Monitoring - {month_en} Update"),
      date_header = glue("{date_fmt} - Trigger status:"),
      status = status_html(activated, "en"),
      description_title = "Trigger Description",
      description_content = desc_en,
      contact_info = "Contact the OCHA Centre for Humanitarian Data via Leonardo Milano, Team Lead\nfor Data Science at leonardo.milano@un.org with any questions or feedback.",
      data_source = "ECMWF SEAS5",
      ref_github = "Full documentation and source code can be found in the [GitHub repository](https://github.com/OCHA-DAP/ds-aa-lac-dry-corridor)."
    ),
    es = list(
      title = "Acci\u00f3n Anticipatoria - StartNetwork Guatemala",
      subtitle = glue("Monitoreo de Sequ\u00eda {season} 2026 - Actualizaci\u00f3n de {month_es}"),
      date_header = glue("{date_fmt_es} - Estado de activaci\u00f3n:"),
      status = status_html(activated, "es"),
      description_title = "Descripci\u00f3n del Mecanismo de Activaci\u00f3n",
      description_content = desc_es,
      contact_info = "Contacte al Centro de Datos Humanitarios de OCHA a trav\u00e9s de Leonardo Milano, Team Lead\nfor Data Science en leonardo.milano@un.org para cualquier pregunta o comentario.",
      data_source = "ECMWF SEAS5",
      ref_github = "La documentaci\u00f3n completa y el c\u00f3digo fuente se encuentran en el [repositorio de GitHub](https://github.com/OCHA-DAP/ds-aa-lac-dry-corridor)."
    )
  )
}


# GT table builders -------------------------------------------------------

#' Build threshold gt table
#'
#' @param df_status data.frame with adm0_es, AOI (optional), value, value_empirical, status
#' @param programme "ocha" or "startnetwork"
#' @param season "Primera" or "Postrera"
#' @param lang "en" or "es"
#' @return gt object
#' @export
build_threshold_gt <- function(df_status, programme, season, lang = "en") {
  is_sn <- programme == "startnetwork"

  if (is_sn) {
    df_tbl <- df_status |> select(adm0_es, AOI, value, value_empirical, status)
  } else {
    df_tbl <- df_status |> select(adm0_es, value, value_empirical, status)
  }

  if (lang == "es") {
    country_lab <- "Pa\u00eds"
    rainfall_lab <- "Precipitaci\u00f3n (mm)"
    threshold_lab <- "Umbral"
    status_lab <- "Estado"
    aoi_lab <- "\u00c1rea de Inter\u00e9s"
    title_prefix <- if (is_sn) "StartNetwork: " else ""
    title <- glue("{title_prefix}Precipitaci\u00f3n Prevista para {season} y Umbrales de Activaci\u00f3n")
    footnote <- if (is_sn) SN_RP_FOOTNOTE_ES else OCHA_RP_FOOTNOTE_ES
  } else {
    country_lab <- "Country"
    rainfall_lab <- "Rainfall (mm)"
    threshold_lab <- "Threshold"
    status_lab <- "Status"
    aoi_lab <- "Area of Interest"
    title_prefix <- if (is_sn) "StartNetwork: " else ""
    title <- glue("{title_prefix}Predicted {season} Rainfall and Trigger Thresholds")
    footnote <- if (is_sn) SN_RP_FOOTNOTE_EN else OCHA_RP_FOOTNOTE_EN
  }

  col_labels <- list(
    adm0_es = country_lab,
    value = rainfall_lab,
    value_empirical = threshold_lab,
    status = status_lab
  )
  if (is_sn) col_labels$AOI <- aoi_lab

  gt_obj <- df_tbl |>
    gt$gt() |>
    gt$cols_label(.list = col_labels) |>
    gt$fmt_number(columns = c("value", "value_empirical"), decimals = 0) |>
    gt$tab_header(title = title) |>
    gt$tab_footnote(footnote = footnote) |>
    gt$tab_options(
      table.font.size = 14,
      heading.background.color = CHD_GREEN,
      table.width = gt$pct(80)
    )

  gt_obj
}


#' Build AOI admin-1 summary gt table
#'
#' @param gdf_adm1 sf object with adm1_pcode, adm0_es, adm1_es columns
#' @param df_aoi data.frame with pcode column (OCHA AOI pcodes)
#' @param lang "en" or "es"
#' @return gt object
#' @export
build_aoi_gt <- function(gdf_adm1, df_aoi, lang = "en") {
  gdf_filtered <- gdf_adm1 |>
    filter(adm1_pcode %in% df_aoi$pcode)

  slv_label <- if (lang == "es") "Nacional (admin 0)" else "National (admin 0)"

  df_tbl <- gdf_filtered |>
    sf$st_drop_geometry() |>
    group_by(adm0_es) |>
    summarise(admin_1 = glue_collapse(adm1_es, sep = ", ")) |>
    bind_rows(tibble(adm0_es = "El Salvador", admin_1 = slv_label))

  if (lang == "es") {
    country_lab <- "Pa\u00eds"
    admin_lab <- "Admin 1"
    title <- "Unidades Admin 1 incluidas en el monitoreo por pa\u00eds"
  } else {
    country_lab <- "Country"
    admin_lab <- "Admin 1"
    title <- "Admin 1 units included in monitoring by country"
  }

  df_tbl |>
    gt$gt() |>
    gt$cols_label(adm0_es = country_lab, admin_1 = admin_lab) |>
    gt$tab_header(title = title) |>
    gt$cols_align(align = "left") |>
    gt$tab_options(
      heading.background.color = CHD_GREEN,
      column_labels.background.color = "#D2F2F0",
      table.font.size = 14,
      table.width = gt$pct(80)
    )
}


# Send helper -------------------------------------------------------------

#' Render and send a monitoring email
#'
#' @param template character path to Rmd template
#' @param subject character email subject line
#' @param render_env environment for render_email (usually parent.frame())
#' @param recipients data.frame or list of data.frames with $email column
#' @param email_list character email list name (prepends "TEST: " when != "full_list")
#' @param credentials blastula credentials object
#' @export
send_monitoring_email <- function(template, subject, render_env, recipients,
                                  email_list, credentials) {
  logger$log_info(glue("Knitting email: {template}"))
  knitted <- render_email(input = template, envir = render_env)

  final_subject <- if (email_list != "full_list") paste0("TEST: ", subject) else subject

  logger$log_info(glue("Sending email: {template}"))
  if (email_list == "full_list") {
    recipients |>
      purrr$map(\(dfet) {
        smtp_send(
          email = knitted,
          from = "data.science@humdata.org",
          to = dfet$email,
          subject = final_subject,
          credentials = credentials,
          verbose = TRUE
        )
      })
  } else {
    smtp_send(
      email = knitted,
      from = "data.science@humdata.org",
      to = recipients$email,
      subject = final_subject,
      credentials = credentials,
      verbose = TRUE
    )
  }
}
