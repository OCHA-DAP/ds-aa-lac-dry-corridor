#' Monitoring of SEAS5 forecasts for CADC AA Framework - 2026 Season
#'
#' Key differences from 2025:
#'   - SEAS5 only (no INSIVUMEH)
#'   - Updated AOIs: GTM expanded (GT20/GT21/GT02/GT19), SLV at admin 0
#'   - StartNetwork (GT14/GT15) included alongside OCHA countries
#'   - Thresholds from ch11 optimization (RP 2.9 for OCHA, RP 4.4 for StartNetwork)
#'   - Threshold parquet includes `programme` column ("ocha" / "startnetwork")
#'
#' The thresholds parquet serves two purposes:
#'   a. provides the thresholds at each activation moment
#'   b. is a config file which tells the code which forecasts to monitor

box::use(
  dplyr[...],
  forcats[...],
  glue[...],
  purrr[...],
  readr,
  stringr,
  tidyr,
  cumulus,
  lubridate,
  logger,
  sf,
  gghdx,
  ggplot2[...],
)

gghdx$gghdx()
box::purge_cache()
box::use(
  utils = ../utils/gen_utils_2026,
  eu26 = ../utils/email_utils_2026,
  ../utils/map
)


# Configuration -----------------------------------------------------------
# core_developer
# developers
# internal_chd
# full_list
EMAIL_LIST <- Sys.getenv("EMAIL_WHO", unset = "core_developer")

# Run date: defaults to Sys.Date(), but can be overridden via RUN_YEAR + RUN_MONTH
# env vars (set in GHA workflow_dispatch or locally for testing).
run_year <- Sys.getenv("RUN_YEAR", unset = "")
run_month <- Sys.getenv("RUN_MONTH", unset = "")
# run_year <- 2014
# run_month <- 3

if (nzchar(run_year) && nzchar(run_month)) {
  run_date_set <- lubridate$make_date(as.integer(run_year), as.integer(run_month), 1L)
} else {
  run_date_set <- as.Date(lubridate$now("UTC"))
}

current_moment <- lubridate$floor_date(run_date_set, "month")

logger$log_info(paste0("EMAIL_LIST = ", EMAIL_LIST))
logger$log_info(paste0("Run date set = ", run_date_set))
logger$log_info(paste0("Current moment = ", current_moment))

email_distribution_list_ocha <- eu26$load_distribution_list(email_list = EMAIL_LIST, framework = "ocha")
email_distribution_list_sn <- eu26$load_distribution_list(email_list = EMAIL_LIST, framework = "startnetwork")

# Loading base data -------------------------------------------------------

# OCHA AOI (HND + GTM admin 1 pcodes; SLV handled separately at adm0)
df_aoi_ocha <- utils$load_aoi_df(version = "2026")
# StartNetwork AOI
df_aoi_sn <- utils$load_aoi_df(version = "2026_startnetwork")

gdf_adm1 <- utils$load_adm1_sf()

# Threshold table
df_thresholds <- cumulus$blob_read(
  container = "projects",
  name = "ds-aa-lac-dry-corridor/monitoring/2026/df_thresholds_2026.parquet"
)

# Filter to current monitoring month
df_relevant_thresholds <- df_thresholds |>
  filter(
    as.character(issued_month_label) == as.character(lubridate$month(current_moment, abbr = TRUE, label = TRUE))
  )

if (nrow(df_relevant_thresholds) == 0) {
  logger$log_warn("No thresholds for current month — nothing to monitor.")
  stop("No thresholds found for the current monitoring month.")
}


# Loading forecasts -------------------------------------------------------

logger$log_info("Getting latest SEAS5 forecast from Postgres")

con <- cumulus$pg_con()
on.exit(DBI::dbDisconnect(con))

current_window <- unique(df_relevant_thresholds$window)

months_to_aggregate <- switch(
  current_window,
  "primera" = 5:8,
  "postrera" = 9:11
)

# ── OCHA forecasts: HND + GTM at admin 1, SLV at admin 0 ──

# HND + GTM (admin 1 weighted mean)
ocha_pcodes <- df_aoi_ocha$pcode
df_seas5_adm1 <- tbl(con, "seas5") |>
  filter(
    adm_level == 1,
    pcode %in% ocha_pcodes,
    issued_date == current_moment
  ) |>
  collect() |>
  mutate(value = lubridate$days_in_month(valid_date) * mean)

df_weights <- tbl(con, "polygon") |>
  filter(adm_level == 1, pcode %in% ocha_pcodes) |>
  select(pcode, iso3, seas5_n_upsampled_pixels) |>
  collect()

df_ocha_adm1 <- cumulus$seas5_aggregate_forecast(
  df_seas5_adm1,
  value = "value",
  valid_months = months_to_aggregate,
  by = c("iso3", "pcode", "issued_date")
) |>
  left_join(df_weights, by = c("iso3", "pcode")) |>
  group_by(iso3, issued_date, leadtime) |>
  summarise(
    value = weighted.mean(value, w = seas5_n_upsampled_pixels),
    .groups = "drop"
  )

# SLV (admin 0)
df_seas5_slv <- tbl(con, "seas5") |>
  filter(iso3 == "SLV", adm_level == 0, issued_date == current_moment) |>
  collect() |>
  mutate(value = lubridate$days_in_month(valid_date) * mean)

df_ocha_slv <- cumulus$seas5_aggregate_forecast(
  df_seas5_slv,
  value = "value",
  valid_months = months_to_aggregate,
  by = c("iso3", "issued_date")
)

df_forecast_ocha <- bind_rows(df_ocha_adm1, df_ocha_slv) |>
  mutate(
    forecast_source = "SEAS5",
    programme = "ocha"
  )


# ── StartNetwork forecasts: GT14/GT15 with split-AOI by leadtime ──
# Per ch11: merged GT14+GT15 (area-weighted) for shorter leadtimes,
#           GT15 (Baja Verapaz) only for long leadtime where merged skill is inadequate.
# The split is driven by the AOI column in the threshold parquet.

sn_pcodes <- df_aoi_sn$pcode
df_sn_thresh <- df_relevant_thresholds |> filter(programme == "startnetwork")

if (nrow(df_sn_thresh) > 0) {
  df_seas5_sn <- tbl(con, "seas5") |>
    filter(
      adm_level == 1,
      pcode %in% sn_pcodes,
      issued_date == current_moment
    ) |>
    collect() |>
    mutate(value = lubridate$days_in_month(valid_date) * mean)

  df_weights_sn <- tbl(con, "polygon") |>
    filter(adm_level == 1, pcode %in% sn_pcodes) |>
    select(pcode, iso3, seas5_n_upsampled_pixels) |>
    collect()

  # Aggregate per-pcode first (needed for both merged and single-pcode paths)
  df_sn_agg <- cumulus$seas5_aggregate_forecast(
    df_seas5_sn,
    value = "value",
    valid_months = months_to_aggregate,
    by = c("iso3", "pcode", "issued_date")
  )

  # Determine which LTs use merged AOI vs Baja Verapaz-only
  merged_lts <- df_sn_thresh |>
    filter(AOI == "Quiche, Baja Verapaz") |> pull(leadtime)
  bv_only_lts <- df_sn_thresh |>
    filter(AOI == "Baja Verapaz") |> pull(leadtime)

  sn_parts <- list()

  # Merged GT14+GT15 weighted mean
  if (length(merged_lts) > 0) {
    sn_parts <- c(sn_parts, list(
      df_sn_agg |>
        left_join(df_weights_sn, by = c("iso3", "pcode")) |>
        group_by(iso3, issued_date, leadtime) |>
        summarise(
          value = weighted.mean(value, w = seas5_n_upsampled_pixels),
          .groups = "drop"
        ) |>
        filter(leadtime %in% merged_lts)
    ))
  }

  # GT15 (Baja Verapaz) only
  if (length(bv_only_lts) > 0) {
    sn_parts <- c(sn_parts, list(
      df_sn_agg |>
        filter(pcode == "GT15") |>
        select(-pcode) |>
        filter(leadtime %in% bv_only_lts)
    ))
  }

  df_forecast_sn <- bind_rows(sn_parts) |>
    mutate(
      forecast_source = "SEAS5",
      programme = "startnetwork"
    )
} else {
  df_forecast_sn <- tibble()
}

# Combine all forecasts
df_forecast <- bind_rows(df_forecast_ocha, df_forecast_sn)


# Assessing activation ----------------------------------------------------

df_forecast_status <- df_forecast |>
  inner_join(
    df_relevant_thresholds,
    by = c("iso3", "leadtime", "forecast_source", "programme")
  ) |>
  mutate(
    status_lgl = value <= value_empirical,
    status = if_else(status_lgl, "Threshold Met", "Threshold Not Met"),
    status = fct_expand(factor(status), "Threshold Met", "Threshold Not Met")
  )

df_forecast_status |> glimpse()


# Split OCHA vs StartNetwork for separate reporting
df_status_ocha <- df_forecast_status |> filter(programme == "ocha")
df_status_sn <- df_forecast_status |> filter(programme == "startnetwork")


# Preparing email content -------------------------------------------------

season <- stringr$str_to_title(unique(df_status_ocha$window))
month_chr <- as.character(lubridate$month(run_date_set, abbr = FALSE, label = TRUE))
monitored_range <- ifelse(season == "Primera", "May-August", "September-November")

# Activation summary logging
df_activations_ocha <- df_status_ocha |> filter(status_lgl)
df_activations_sn <- df_status_sn |> filter(status_lgl)

logger$log_info(sprintf(
  "OCHA thresholds met: %s | StartNetwork thresholds met: %s",
  if (nrow(df_activations_ocha) > 0) paste(unique(df_activations_ocha$adm0_es), collapse = ", ") else "None",
  if (nrow(df_activations_sn) > 0) "Guatemala (StartNetwork)" else "None"
))

# Build bilingual email text
email_txt <- eu26$build_email_text_ocha(df_status_ocha, run_date_set, season, monitored_range)
email_txt_sn <- eu26$build_email_text_sn(df_status_sn, run_date_set, season, monitored_range)

# Build gt tables (en + es)
gt_threshold_ocha_en <- eu26$build_threshold_gt(df_status_ocha, "ocha", season, "en")
gt_threshold_ocha_es <- eu26$build_threshold_gt(df_status_ocha, "ocha", season, "es")
gt_threshold_sn_en <- eu26$build_threshold_gt(df_status_sn, "startnetwork", season, "en")
gt_threshold_sn_es <- eu26$build_threshold_gt(df_status_sn, "startnetwork", season, "es")
gt_aoi_en <- eu26$build_aoi_gt(gdf_adm1, df_aoi_ocha, "en")
gt_aoi_es <- eu26$build_aoi_gt(gdf_adm1, df_aoi_ocha, "es")

# Admin 1 subset needed for map
gdf_adm1_ocha <- gdf_adm1 |>
  filter(adm1_pcode %in% df_aoi_ocha$pcode)


# Map generation ----------------------------------------------------------

# Dissolve OCHA admin1 to country level for map
gdf_aoi_country <- bind_rows(
  gdf_adm1_ocha |>
    group_by(adm0_es) |>
    summarise(do_union = TRUE),
  # SLV: use admin0 boundary
  gdf_adm1 |>
    filter(stringr$str_detect(adm1_pcode, "^SV")) |>
    group_by(adm0_es) |>
    summarise(do_union = TRUE)
)

gdf_adm0_status <- gdf_aoi_country |>
  left_join(
    df_status_ocha |> select(adm0_es, status)
  )

logger$log_info("Loading Map layers from blob")
l_gdf_simple <- map$load_simplified_map_layers()

# Move Nicaragua from AOI to surrounding (same as 2025)
ni_row <- l_gdf_simple$AOI_ADM0 |> filter(adm0_pcode == "NI")
l_gdf_simple$AOI_ADM0 <- l_gdf_simple$AOI_ADM0 |> filter(adm0_pcode != "NI")
l_gdf_simple$AOI_SURROUNDING <- bind_rows(l_gdf_simple$AOI_SURROUNDING, ni_row)

logger$log_info("Making Map")
m_choro <- map$trigger_status_choropleth(
  gdf_aoi = gdf_adm0_status,
  gdf_adm1 = l_gdf_simple$AOI_ADM1,
  gdf_adm0_surrounding = l_gdf_simple$AOI_SURROUNDING,
  gdf_adm0 = l_gdf_simple$AOI_ADM0,
  insivumeh_data_available = TRUE,
  aoi_txt_label_size = 8,
  run_date = run_date_set
)


# Rainfall plot -----------------------------------------------------------

logger$log_info("Making rainfall plot")

p_rainfall <- df_status_ocha |>
  ggplot(
    aes(x = adm0_es, y = value)
  ) +
  geom_point(
    aes(color = status),
    show.legend = c(color = TRUE)
  ) +
  scale_color_manual(
    values = c(
      `Threshold Not Met` = "#55b284ff",
      `Threshold Met` = "#F2645A"
    ),
    drop = FALSE
  ) +
  geom_hline(
    aes(yintercept = value_empirical),
    linetype = "dashed",
    color = "tomato"
  ) +
  scale_y_continuous(
    limits = c(0, max(df_status_ocha$value, df_status_ocha$value_empirical)),
    expand = expansion(mult = c(0, 0.1))
  ) +
  facet_wrap(
    ~adm0_es,
    scales = "free_x",
    nrow = 1, ncol = 3
  ) +
  labs(
    title = glue("CADC Drought Monitoring - Forecasted {season} Rainfall ({monitored_range} {format(run_date_set, '%Y')})"),
    subtitle = glue("Forecast Published: {format(run_date_set, '%Y')} {month_chr}"),
    y = "Rainfall (mm)",
    caption = "Horizontal red dashed lines indicate trigger threshold level."
  ) +
  theme(
    axis.title.x = element_blank(),
    title = element_text(size = 16),
    plot.subtitle = element_text(size = 16),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    axis.text.y = element_text(angle = 90, size = 15),
    strip.text = element_text(size = 16),
    axis.text.x = element_blank(),
    plot.caption = element_text(hjust = 0, size = 14)
  )


# Email delivery ----------------------------------------------------------

data_accessed <- trimws(format(as.Date(current_moment), "%B %Y"))

# Convert plots to base64 inline images
map_b64 <- eu26$ggplot_to_base64(m_choro, width = 8, height = 5.5)
rainfall_b64 <- eu26$ggplot_to_base64(p_rainfall, width = 8, height = 5.5)

# Build OCHA email HTML content
ocha_html <- eu26$build_email_html_ocha(
  email_txt = email_txt,
  gt_threshold_en = gt_threshold_ocha_en,
  gt_threshold_es = gt_threshold_ocha_es,
  gt_aoi_en = gt_aoi_en,
  gt_aoi_es = gt_aoi_es,
  map_b64 = map_b64,
  rainfall_b64 = rainfall_b64,
  data_accessed = data_accessed
)

# ── Send OCHA email ──
eu26$send_monitoring_email(
  subject = email_txt$subj,
  body_html = ocha_html,
  distribution_list = email_distribution_list_ocha,
  email_list = EMAIL_LIST
)

# ── Send StartNetwork email ──
if (nrow(df_status_sn) > 0) {
  sn_html <- eu26$build_email_html_sn(
    email_txt = email_txt_sn,
    gt_threshold_en = gt_threshold_sn_en,
    gt_threshold_es = gt_threshold_sn_es,
    data_accessed = data_accessed
  )

  eu26$send_monitoring_email(
    subject = email_txt_sn$subj,
    body_html = sn_html,
    distribution_list = email_distribution_list_sn,
    email_list = EMAIL_LIST
  )
}
