#' Post-Activation Informational Monitoring Update
#'
#' Generates informational content for manual emails to colleagues after
#' the CADC drought trigger activated in all 3 OCHA countries + StartNetwork
#' based on the March 2026 SEAS5 forecast.
#'
#' Produces separate OCHA and StartNetwork outputs:
#'   1. Jitter plot: current forecast vs historical hindcast distribution per AOI
#'      - forecast point labeled with mm (left) and % below normal (right)
#'      - historical median labeled
#'   2. Summary gt table: forecast, historical average, % below normal
#'   3. AOI admin-1 listing table
#'   4. Bilingual email text templates (EN + ES for OCHA; EN for StartNetwork)
#'
#' Usage: set `current_moment` below, then source/run interactively.

library(dplyr)
library(lubridate)
library(glue)
library(ggplot2)
library(gt)
library(cumulus)
library(gghdx)

gghdx()


# 0. Configuration --------------------------------------------------------

# Set the forecast issue month (first of month).
# Override for testing past months: as.Date("2026-03-01")
current_moment <- floor_date(Sys.Date(), "month")
# current_moment <- as.Date("2026-03-01")

current_month <- month(current_moment)
current_year <- year(current_moment)

# Auto-detect season
if (current_month %in% 3:5) {
  season <- "primera"
  valid_months <- 5:8
  issued_months <- c(3, 4, 5)
  monitored_range_en <- "May\u2013August"
  monitored_range_es <- "mayo\u2013agosto"
  season_months_abbr <- "MJJA"
} else if (current_month %in% 6:9) {
  season <- "postrera"
  valid_months <- 9:11
  issued_months <- c(6, 7, 8, 9)
  monitored_range_en <- "September\u2013November"
  monitored_range_es <- "septiembre\u2013noviembre"
  season_months_abbr <- "SON"
} else {
  stop("Current month (", current_month, ") is outside the monitoring window (March-September).")
}

season_label <- tools::toTitleCase(season)
month_label_en <- format(current_moment, "%B")

month_names_es <- c(
  "enero", "febrero", "marzo", "abril", "mayo", "junio",
  "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"
)
month_label_es <- tools::toTitleCase(month_names_es[current_month])

cat(glue("Season: {season_label} | Issued: {month_label_en} {current_year}"), "\n")


# 1. AOI definitions (inlined) --------------------------------------------

BASELINE_START <- 1991
BASELINE_END <- 2024

# OCHA AOIs
ocha_configs <- list(
  HND = list(
    label     = "Honduras",
    pcodes    = c("HN07", "HN08"),
    adm_level = 1,
    iso3      = "HND",
    admin1    = "El Para\u00edso, Francisco Moraz\u00e1n"
  ),
  GTM = list(
    label     = "Guatemala",
    pcodes    = c("GT20", "GT21", "GT02", "GT19"),
    adm_level = 1,
    iso3      = "GTM",
    admin1    = "Chiquimula, Jalapa, El Progreso, Zacapa"
  ),
  SLV = list(
    label     = "El Salvador",
    pcodes    = NULL,
    adm_level = 0,
    iso3      = "SLV",
    admin1    = "National (admin 0)"
  )
)

# StartNetwork AOI
sn_configs <- list(
  SN = list(
    label     = "Guatemala\n(Start Network)",
    pcodes    = c("GT14", "GT15"),
    adm_level = 1,
    iso3      = "GTM",
    admin1    = "Quich\u00e9, Baja Verapaz"
  )
)

all_configs <- c(ocha_configs, sn_configs)


# 2. DB connection + load SEAS5 + weights ---------------------------------

con <- pg_con()
on.exit(DBI::dbDisconnect(con))

all_adm1_pcodes <- unlist(
  lapply(all_configs, \(cfg) if (cfg$adm_level == 1) cfg$pcodes else NULL)
)

cat("Loading SEAS5 admin-1 data...\n")
df_seas5_adm1 <- tbl(con, "seas5") |>
  mutate(across(pcode, as.character)) |>
  filter(adm_level == 1, pcode %in% all_adm1_pcodes) |>
  collect() |>
  mutate(value_mm = days_in_month(valid_date) * mean)

cat("Loading SEAS5 admin-0 data (SLV)...\n")
df_seas5_slv <- tbl(con, "seas5") |>
  filter(iso3 == "SLV", adm_level == 0) |>
  collect() |>
  mutate(value_mm = days_in_month(valid_date) * mean)

df_weights <- tbl(con, "polygon") |>
  mutate(across(pcode, as.character)) |>
  filter(adm_level == 1, pcode %in% all_adm1_pcodes) |>
  select(pcode, seas5_n_upsampled_pixels) |>
  collect()


# 3. Seasonal aggregation + area weighting --------------------------------

aggregate_aoi <- function(cfg, df_adm1, df_adm0_slv, df_wts) {
  if (cfg$adm_level == 1) {
    df_raw <- df_adm1 |> filter(pcode %in% cfg$pcodes)
    by_cols <- c("iso3", "pcode", "issued_date")
  } else {
    df_raw <- df_adm0_slv
    by_cols <- c("iso3", "issued_date")
  }
  
  df_agg <- seas5_aggregate_forecast(
    df_raw,
    value = "value_mm",
    valid_months = valid_months,
    by = by_cols
  ) |>
    rename(fcst_mm = value_mm) |>
    mutate(
      year = year(issued_date),
      issued_month = month(issued_date)
    ) |>
    filter(issued_month %in% issued_months)
  
  if (cfg$adm_level == 1 && length(cfg$pcodes) > 1) {
    df_agg <- df_agg |>
      left_join(df_wts, by = "pcode") |>
      group_by(year, leadtime, issued_date) |>
      summarise(
        fcst_mm = weighted.mean(fcst_mm, w = seas5_n_upsampled_pixels),
        .groups = "drop"
      )
  } else if (cfg$adm_level == 1) {
    df_agg <- df_agg |> select(-pcode)
  }
  
  df_agg |> mutate(aoi_label = cfg$label)
}

cat("Aggregating seasonal forecasts per AOI...\n")
df_all_aois <- bind_rows(
  lapply(all_configs, aggregate_aoi,
         df_adm1 = df_seas5_adm1,
         df_adm0_slv = df_seas5_slv,
         df_wts = df_weights)
)


# 4. Split hindcast vs current, filter to same leadtime -------------------

df_current_raw <- df_all_aois |>
  filter(issued_date == current_moment)

if (nrow(df_current_raw) == 0) {
  stop("No forecast data found for ", current_moment,
       ". The forecast may not be ingested yet.")
}

current_lts <- df_current_raw |> distinct(aoi_label, leadtime)
cat("\nCurrent forecast leadtimes:\n")
print(current_lts)

df_hindcast <- df_all_aois |>
  filter(year >= BASELINE_START, year <= BASELINE_END) |>
  semi_join(current_lts, by = c("aoi_label", "leadtime"))

df_current <- df_current_raw

# Compute stats needed for labels
df_hist_stats <- df_hindcast |>
  group_by(aoi_label) |>
  summarise(
    hist_mean   = mean(fcst_mm),
    hist_median = median(fcst_mm),
    .groups     = "drop"
  )

df_current <- df_current |>
  left_join(df_hist_stats, by = "aoi_label") |>
  mutate(
    pct_below = round((1 - fcst_mm / hist_mean) * 100, 0),
    label_left = paste0(round(fcst_mm, 0), " mm")
  )

# Split into OCHA and StartNetwork
ocha_labels <- sapply(ocha_configs, \(x) x$label)
sn_labels <- sapply(sn_configs, \(x) x$label)

df_hindcast_ocha <- df_hindcast |> filter(aoi_label %in% ocha_labels)
df_current_ocha  <- df_current  |> filter(aoi_label %in% ocha_labels)
df_hindcast_sn   <- df_hindcast |> filter(aoi_label %in% sn_labels)
df_current_sn    <- df_current  |> filter(aoi_label %in% sn_labels)

# Factor levels for plot ordering
ocha_order <- c("Honduras", "El Salvador", "Guatemala")
df_hindcast_ocha <- df_hindcast_ocha |> mutate(aoi_label = factor(aoi_label, levels = ocha_order))
df_current_ocha  <- df_current_ocha  |> mutate(aoi_label = factor(aoi_label, levels = ocha_order))

cat(glue("\nOCHA hindcast: {nrow(df_hindcast_ocha)} rows | current: {nrow(df_current_ocha)}"), "\n")
cat(glue("SN hindcast: {nrow(df_hindcast_sn)} rows | current: {nrow(df_current_sn)}"), "\n")


# 4b. Would-be trigger thresholds -----------------------------------------
# Same source + keys as the activation-status monitoring script
# (update_activation_status.R). `value_empirical` is the threshold; at a given
# issue month there is exactly one threshold row (one leadtime) per AOI.

cat("Loading would-be trigger thresholds...\n")
df_thresholds <- cumulus::blob_read(
  container = "projects",
  name = "ds-aa-lac-dry-corridor/monitoring/2026/df_thresholds_2026.parquet"
)

# Map each AOI label to its threshold key (iso3 + programme)
df_aoi_meta <- bind_rows(
  tibble(
    aoi_label = sapply(ocha_configs, \(x) x$label),
    iso3      = sapply(ocha_configs, \(x) x$iso3),
    programme = "ocha"
  ),
  tibble(
    aoi_label = sapply(sn_configs, \(x) x$label),
    iso3      = sapply(sn_configs, \(x) x$iso3),
    programme = "startnetwork"
  )
)

current_month_abbr <- as.character(month(current_moment, label = TRUE, abbr = TRUE))

df_thresh_current <- df_thresholds |>
  filter(
    window == season,
    as.character(issued_month_label) == current_month_abbr
  ) |>
  inner_join(df_aoi_meta, by = c("iso3", "programme")) |>
  select(aoi_label, leadtime, thresh_mm = value_empirical)

if (nrow(df_thresh_current) == 0) {
  warning("No thresholds matched current season/month (", season, " / ",
          current_month_abbr, ") — threshold plots will omit the purple line.")
}

df_thresh_ocha <- df_thresh_current |>
  filter(aoi_label %in% ocha_labels) |>
  mutate(aoi_label = factor(aoi_label, levels = ocha_order))
df_thresh_sn <- df_thresh_current |> filter(aoi_label %in% sn_labels)


# 5. Plot helper -----------------------------------------------------------

# Purple used for the would-be trigger threshold line
THRESH_COLOR <- "#7E3F98"

build_jitter_plot <- function(df_hist, df_curr, title_suffix = "", df_thresh = NULL) {
  df_plot <- bind_rows(
    df_hist |> mutate(point_type = "hist"),
    df_curr |> mutate(point_type = "current")
  )

  # Mean stats for label (consistent with gt table)
  df_mean <- df_hist |>
    group_by(aoi_label) |>
    summarise(mean_mm = mean(fcst_mm), .groups = "drop")

  show_thresh <- !is.null(df_thresh) && nrow(df_thresh) > 0

  p <- ggplot(df_plot, aes(x = aoi_label, y = fcst_mm)) +
    geom_jitter(
      data = \(d) filter(d, point_type == "hist"),
      width = 0.15, alpha = 0.35, color = "grey55", size = 2.5
    ) +
    # Mean crossbar
    geom_crossbar(
      data = df_mean,
      aes(x = aoi_label, y = mean_mm, ymin = mean_mm, ymax = mean_mm),
      width = 0.35, color = "#1EBFB3", linewidth = 0.6, fatten = 2,
      inherit.aes = FALSE
    ) +
    # Mean label
    geom_text(
      data = df_mean,
      aes(x = aoi_label, y = mean_mm, label = paste0("avg: ", round(mean_mm, 0), " mm")),
      hjust = -0.15, vjust = -0.8, size = 3.2, color = "#1EBFB3", fontface = "italic",
      inherit.aes = FALSE
    )

  # Would-be trigger threshold crossbar + label (optional)
  if (show_thresh) {
    p <- p +
      geom_crossbar(
        data = df_thresh,
        aes(x = aoi_label, y = thresh_mm, ymin = thresh_mm, ymax = thresh_mm),
        width = 0.35, color = THRESH_COLOR, linewidth = 0.6, fatten = 2,
        inherit.aes = FALSE
      ) +
      geom_text(
        data = df_thresh,
        aes(x = aoi_label, y = thresh_mm, label = paste0("threshold: ", round(thresh_mm, 0), " mm")),
        hjust = -0.15, vjust = 1.6, size = 3.2, color = THRESH_COLOR, fontface = "italic",
        inherit.aes = FALSE
      )
  }

  p <- p +
    # Current forecast point
    geom_point(
      data = \(d) filter(d, point_type == "current"),
      color = "#F2645A", size = 5, shape = 18
    ) +
    # Label: mm on the left
    geom_text(
      data = df_curr,
      mapping = aes(x = aoi_label, y = fcst_mm, label = label_left),
      hjust = 1.2, vjust = 0.4, size = 3.5, color = "#F2645A", fontface = "bold",
      inherit.aes = FALSE
    ) +
    facet_wrap(
      ~aoi_label,
      # scales = "free_y",
      nrow = 1
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(
      title = glue(
        "Current & Historical Rainfall {month_label_en} Published Forecasts for {season_label} Season ({season_months_abbr}){title_suffix}"
      ),
      subtitle = glue(
        "Current forecast published {month_label_en} {current_year} shown in red"
      ),
      y = "Seasonal Rainfall (mm)",
      caption = glue(
        "All forecasted {season_months_abbr} rainfall values from historical forecasts (same leadtime) {BASELINE_START}\u2013{BASELINE_END} shown as points.\nTeal line: historical average.",
        if (show_thresh) " Purple line: would-be trigger threshold." else ""
      )
    ) +
    theme(
      axis.title.x  = element_blank(),
      axis.text.x    = element_blank(),
      axis.ticks.x   = element_blank(),
      title          = element_text(size = 14),
      plot.subtitle  = element_text(size = 13),
      strip.text     = element_text(size = 13),
      plot.caption   = element_text(hjust = 0, size = 11)
    )

  p
}


# 5a. OCHA plot
p_ocha <- build_jitter_plot(df_hindcast_ocha, df_current_ocha)
print(p_ocha)

# 5a-thresh. OCHA plot WITH would-be trigger threshold line (purple)
# Custom facet order for this variant: Guatemala, Honduras, El Salvador.
ocha_order_thresh <- c("Guatemala", "Honduras", "El Salvador")
relevel_ocha <- function(df) {
  df |> mutate(aoi_label = factor(aoi_label, levels = ocha_order_thresh))
}
p_ocha_thresh <- build_jitter_plot(
  relevel_ocha(df_hindcast_ocha),
  relevel_ocha(df_current_ocha),
  df_thresh = relevel_ocha(df_thresh_ocha)
)
print(p_ocha_thresh)

# 5b. StartNetwork plot (only if SN data exists for this season)
if (nrow(df_current_sn) > 0) {
  p_sn <- build_jitter_plot(df_hindcast_sn, df_current_sn, title_suffix = " \u2014 Start Network")

  # 5b-thresh. StartNetwork plot WITH would-be trigger threshold line (purple)
  p_sn_thresh <- build_jitter_plot(
    df_hindcast_sn, df_current_sn,
    title_suffix = " \u2014 Start Network", df_thresh = df_thresh_sn
  )
}


# 6. Summary gt tables -----------------------------------------------------

build_summary_gt <- function(df_curr, title, footnote, lang = "en") {
  df_tbl <- df_curr |>
    select(aoi_label, fcst_mm, hist_mean, pct_below)
  
  if (lang == "en") {
    labels <- list(
      aoi_label = "Country / AOI",
      fcst_mm   = "Forecasted Rainfall (mm)",
      hist_mean = "Historical Average (mm)",
      pct_below = "% Below Normal"
    )
  } else {
    labels <- list(
      aoi_label = "Pa\u00eds / \u00c1rea de Inter\u00e9s",
      fcst_mm   = "Precipitaci\u00f3n Prevista (mm)",
      hist_mean = "Promedio Hist\u00f3rico (mm)",
      pct_below = "% Por Debajo del Normal"
    )
  }
  
  df_tbl |>
    gt() |>
    cols_label(.list = labels) |>
    fmt_number(columns = c(fcst_mm, hist_mean), decimals = 0) |>
    tab_header(title = title) |>
    tab_footnote(footnote) |>
    tab_options(
      table.font.size          = 14,
      heading.background.color = "#55b284ff",
      table.width              = pct(80)
    )
}

build_aoi_gt <- function(configs, lang = "en") {
  slv_label <- if (lang == "es") "Nacional (admin 0)" else "National (admin 0)"
  
  df_tbl <- tibble(
    country = sapply(configs, \(x) gsub("\n", " ", x$label)),
    admin_1 = sapply(configs, \(x) {
      if (is.null(x$pcodes)) slv_label else x$admin1
    })
  )
  
  if (lang == "en") {
    title <- "Admin 1 units included in monitoring by country"
    country_lab <- "Country"
  } else {
    title <- "Unidades Admin 1 incluidas en el monitoreo por pa\u00eds"
    country_lab <- "Pa\u00eds"
  }
  
  df_tbl |>
    gt() |>
    cols_label(country = country_lab, admin_1 = "Admin 1") |>
    tab_header(title = title) |>
    cols_align(align = "left") |>
    tab_options(
      heading.background.color        = "#55b284ff",
      column_labels.background.color  = "#D2F2F0",
      table.font.size                 = 14,
      table.width                     = pct(80)
    )
}

footnote_en <- glue("Historical average from ECMWF SEAS5 hindcasts ({BASELINE_START}\u2013{BASELINE_END}) at the same forecast leadtime. Negative values indicate forecast is above normal.")
footnote_es <- glue("Promedio hist\u00f3rico de los hindcasts ECMWF SEAS5 ({BASELINE_START}\u2013{BASELINE_END}) al mismo tiempo de anticipaci\u00f3n. Valores negativos indican que el pron\u00f3stico est\u00e1 por encima del normal.")

# ── OCHA tables ──
gt_ocha_en <- build_summary_gt(
  df_current_ocha,
  title = glue("{season_label} Seasonal Rainfall Forecast Summary \u2014 {month_label_en} {current_year}"),
  footnote = footnote_en, lang = "en"
)
gt_ocha_es <- build_summary_gt(
  df_current_ocha,
  title = glue("{season_label}: Resumen del Pron\u00f3stico de Precipitaci\u00f3n \u2014 {month_label_es} {current_year}"),
  footnote = footnote_es, lang = "es"
)
gt_aoi_ocha_en <- build_aoi_gt(ocha_configs, "en")
gt_aoi_ocha_es <- build_aoi_gt(ocha_configs, "es")

# ── StartNetwork tables (EN only) ──
if (nrow(df_current_sn) > 0) {
  gt_sn_en <- build_summary_gt(
    df_current_sn,
    title = glue("Start Network: {season_label} Rainfall Forecast Summary \u2014 {month_label_en} {current_year}"),
    footnote = footnote_en, lang = "en"
  )
  gt_aoi_sn_en <- build_aoi_gt(sn_configs, "en")
}


# 7. Email text templates --------------------------------------------------

# ── OCHA (EN + ES) ──
ocha_aoi_text_en <- paste(
  sapply(ocha_configs, \(x) glue("- {x$label}: {x$admin1}")),
  collapse = "\n"
)
ocha_aoi_text_es <- paste(
  sapply(ocha_configs, \(x) {
    admin_es <- if (is.null(x$pcodes)) "Nacional (admin 0)" else x$admin1
    glue("- {x$label}: {admin_es}")
  }),
  collapse = "\n"
)

email_ocha_en <- glue("
Subject: CADC Drought Monitoring Update \u2014 {season_label} {current_year} \u2014 {month_label_en} Forecast

The CADC anticipatory action frameworks were activated for Honduras, El Salvador, and Guatemala based on the March 2026 SEAS5 forecast. The following is an informational update on the latest seasonal rainfall forecast.

The ECMWF SEAS5 forecast issued in {month_label_en} {current_year} for the {season_label} season ({monitored_range_en}) projects the following seasonal rainfall totals for each area of interest. The chart and table below show how the current forecast compares to the historical range of hindcast values ({BASELINE_START}\u2013{BASELINE_END}) at the same forecast leadtime.

[CHART]

[TABLE]

Areas of interest monitored:
{ocha_aoi_text_en}

Data source: ECMWF SEAS5
Data accessed: {month_label_en} {current_year}
Full documentation: https://github.com/OCHA-DAP/ds-aa-lac-dry-corridor
")

email_ocha_es <- glue("
Asunto: Monitoreo de Sequ\u00eda CADC \u2014 {season_label} {current_year} \u2014 Pron\u00f3stico de {month_label_es}

Los marcos de acci\u00f3n anticipatoria del CADC fueron activados para Honduras, El Salvador y Guatemala con base en el pron\u00f3stico SEAS5 de marzo de 2026. La siguiente es una actualizaci\u00f3n informativa sobre el \u00faltimo pron\u00f3stico de precipitaci\u00f3n estacional.

El pron\u00f3stico ECMWF SEAS5 emitido en {month_label_es} de {current_year} para la temporada de {season_label} ({monitored_range_es}) proyecta los siguientes totales de precipitaci\u00f3n estacional para cada \u00e1rea de inter\u00e9s. El gr\u00e1fico y la tabla a continuaci\u00f3n muestran c\u00f3mo se compara el pron\u00f3stico actual con el rango completo de valores hist\u00f3ricos de hindcast ({BASELINE_START}\u2013{BASELINE_END}) al mismo tiempo de anticipaci\u00f3n.

[GR\u00c1FICO]

[TABLA]

\u00c1reas de inter\u00e9s monitoreadas:
{ocha_aoi_text_es}

Fuente de datos: ECMWF SEAS5
Datos consultados: {month_label_es} de {current_year}
Documentaci\u00f3n completa: https://github.com/OCHA-DAP/ds-aa-lac-dry-corridor
")

# ── StartNetwork (EN only) ──
sn_aoi_text <- paste(
  sapply(sn_configs, \(x) glue("- {gsub(chr(10), ' ', x$label)}: {x$admin1}")),
  collapse = "\n"
)

email_sn_en <- glue("
Subject: Start Network Guatemala \u2014 {season_label} Drought Monitoring Update \u2014 {month_label_en} {current_year}

The Start Network Guatemala anticipatory action framework was activated based on the March 2026 SEAS5 forecast. The following is an informational update on the latest seasonal rainfall forecast.

The ECMWF SEAS5 forecast issued in {month_label_en} {current_year} for the {season_label} season ({monitored_range_en}) projects the following seasonal rainfall for the Start Network area of interest. The chart and table below show how the current forecast compares to the historical range of hindcast values ({BASELINE_START}\u2013{BASELINE_END}) at the same forecast leadtime.

[CHART]

[TABLE]

Area of interest:
{sn_aoi_text}

Data source: ECMWF SEAS5
Data accessed: {month_label_en} {current_year}
Full documentation: https://github.com/OCHA-DAP/ds-aa-lac-dry-corridor
")


# ── Print templates ──

cat("\n\n")
cat("================================================================\n")
cat("  OCHA \u2014 ENGLISH\n")
cat("================================================================\n")
cat(email_ocha_en)

cat("\n\n")
cat("================================================================\n")
cat("  OCHA \u2014 ESPA\u00d1OL\n")
cat("================================================================\n")
cat(email_ocha_es)

cat("\n\n")
cat("================================================================\n")
cat("  START NETWORK \u2014 ENGLISH\n")
cat("================================================================\n")
cat(email_sn_en)
cat("\n")
