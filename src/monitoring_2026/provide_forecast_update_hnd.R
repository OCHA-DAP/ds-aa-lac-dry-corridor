#' SEAS5 Forecast Informational Update — Honduras
#'
#' Honduras-only version of the OCHA jitter plot in
#' provide_informational_update.R, requested by Honduras. Same data and style
#' (current SEAS5 seasonal forecast vs 1991-2024 hindcast distribution at the
#' same leadtime, with the would-be trigger threshold), but a single facet.
#' Companion to the observed ERA5 version in provide_observed_update_hnd.R.
#'
#' Code is borrowed from provide_informational_update.R and trimmed to the
#' Honduras AOI (El Paraiso + Francisco Morazan, area-weighted).
#'
#' Usage: set `current_moment` below, then source/run interactively.

library(dplyr)
library(lubridate)
library(glue)
library(ggplot2)
library(cumulus)
library(gghdx)

gghdx()


# 0. Configuration --------------------------------------------------------

# Forecast issue month (first of month). Override for past months:
# current_moment <- as.Date("2026-07-01")
current_moment <- floor_date(Sys.Date(), "month")

current_month <- month(current_moment)
current_year  <- year(current_moment)

# Auto-detect season from issue month (same rules as the shared script)
if (current_month %in% 3:5) {
  season             <- "primera"
  valid_months       <- 5:8
  issued_months      <- c(3, 4, 5)
  season_months_abbr <- "MJJA"
} else if (current_month %in% 6:9) {
  season             <- "postrera"
  valid_months       <- 9:11
  issued_months      <- c(6, 7, 8, 9)
  season_months_abbr <- "SON"
} else {
  stop("Current month (", current_month, ") is outside the monitoring window (March-September).")
}

season_label   <- tools::toTitleCase(season)
month_label_en <- format(current_moment, "%B")

BASELINE_START <- 1991
BASELINE_END   <- 2024

hnd_cfg <- list(
  label     = "Honduras",
  pcodes    = c("HN07", "HN08"),
  iso3      = "HND",
  programme = "ocha",
  admin1    = "El Paraíso, Francisco Morazán"
)

OUT_PNG <- "src/monitoring_2026/png"

cat(glue("Season: {season_label} | Issued: {month_label_en} {current_year}"), "\n")


# 1. Load SEAS5 + weights -------------------------------------------------

con <- pg_con()
on.exit(DBI::dbDisconnect(con))

cat("Loading SEAS5 admin-1 data (HND)...\n")
df_seas5 <- tbl(con, "seas5") |>
  mutate(across(pcode, as.character)) |>
  filter(adm_level == 1, pcode %in% !!hnd_cfg$pcodes) |>
  collect() |>
  mutate(value_mm = days_in_month(valid_date) * mean)

if (nrow(df_seas5) == 0) stop("No SEAS5 rows returned for HND pcodes ", toString(hnd_cfg$pcodes))

df_weights <- tbl(con, "polygon") |>
  mutate(across(pcode, as.character)) |>
  filter(adm_level == 1, pcode %in% !!hnd_cfg$pcodes) |>
  select(pcode, seas5_n_upsampled_pixels) |>
  collect()

if (nrow(df_weights) != length(hnd_cfg$pcodes) || any(is.na(df_weights$seas5_n_upsampled_pixels))) {
  stop("Missing SEAS5 pixel weights for one or more HND pcodes.")
}


# 2. Seasonal aggregation + area weighting --------------------------------

df_aoi <- seas5_aggregate_forecast(
  df_seas5,
  value        = "value_mm",
  valid_months = valid_months,
  by           = c("iso3", "pcode", "issued_date")
) |>
  rename(fcst_mm = value_mm) |>
  mutate(
    year         = year(issued_date),
    issued_month = month(issued_date)
  ) |>
  filter(issued_month %in% issued_months) |>
  left_join(df_weights, by = "pcode") |>
  group_by(year, leadtime, issued_date) |>
  summarise(
    fcst_mm = weighted.mean(fcst_mm, w = seas5_n_upsampled_pixels),
    .groups = "drop"
  ) |>
  mutate(aoi_label = hnd_cfg$label)


# 3. Split hindcast vs current at the same leadtime -----------------------

df_current <- df_aoi |> filter(issued_date == current_moment)

if (nrow(df_current) != 1) {
  stop("Expected exactly one current forecast row for ", current_moment,
       ", got ", nrow(df_current), ". The forecast may not be ingested yet.")
}

current_lt <- df_current$leadtime
cat(glue("Current forecast leadtime: {current_lt}"), "\n")

df_hindcast <- df_aoi |>
  filter(year >= BASELINE_START, year <= BASELINE_END, leadtime == current_lt)

if (nrow(df_hindcast) != BASELINE_END - BASELINE_START + 1) {
  stop("Hindcast has ", nrow(df_hindcast), " years; expected ",
       BASELINE_END - BASELINE_START + 1)
}

hist_mean <- mean(df_hindcast$fcst_mm)

df_current <- df_current |>
  mutate(
    hist_mean  = hist_mean,
    pct_below  = round((1 - fcst_mm / hist_mean) * 100, 0),
    label_left = paste0(round(fcst_mm, 0), " mm")
  )

cat(glue(
  "{month_label_en} {current_year} forecast ({season_months_abbr}): {round(df_current$fcst_mm)} mm | ",
  "hindcast avg {BASELINE_START}–{BASELINE_END}: {round(hist_mean)} mm | ",
  "{df_current$pct_below}% below normal"
), "\n")


# 4. Would-be trigger threshold -------------------------------------------
# Same source/keys as update_activation_status.R: one threshold row (one
# leadtime) per AOI at a given issue month; `value_empirical` is the threshold.

cat("Loading would-be trigger threshold...\n")
df_thresholds <- cumulus::blob_read(
  container = "projects",
  name = "ds-aa-lac-dry-corridor/monitoring/2026/df_thresholds_2026.parquet"
)

current_month_abbr <- as.character(month(current_moment, label = TRUE, abbr = TRUE))

df_thresh <- df_thresholds |>
  filter(
    window == season,
    as.character(issued_month_label) == current_month_abbr,
    iso3 == hnd_cfg$iso3,
    programme == hnd_cfg$programme
  ) |>
  transmute(aoi_label = hnd_cfg$label, leadtime, thresh_mm = value_empirical)

if (nrow(df_thresh) != 1) {
  stop("Expected one HND threshold row for ", season, " / ", current_month_abbr,
       ", got ", nrow(df_thresh))
}
if (df_thresh$leadtime != current_lt) {
  stop("Threshold leadtime (", df_thresh$leadtime, ") != forecast leadtime (", current_lt, ")")
}


# 5. Plot -----------------------------------------------------------------

THRESH_COLOR <- "#7E3F98"

df_plot <- bind_rows(
  df_hindcast |> mutate(point_type = "hist"),
  df_current  |> select(year, leadtime, issued_date, fcst_mm, aoi_label) |> mutate(point_type = "current")
)
df_mean <- tibble(aoi_label = hnd_cfg$label, mean_mm = hist_mean)

p_hnd <- ggplot(df_plot, aes(x = aoi_label, y = fcst_mm)) +
  geom_jitter(
    data = \(d) filter(d, point_type == "hist"),
    width = 0.15, alpha = 0.35, color = "grey55", size = 2.5
  ) +
  # Historical mean crossbar + label
  geom_crossbar(
    data = df_mean,
    aes(x = aoi_label, y = mean_mm, ymin = mean_mm, ymax = mean_mm),
    width = 0.35, color = "#1EBFB3", linewidth = 0.6, fatten = 2,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = df_mean,
    aes(x = aoi_label, y = mean_mm, label = paste0("avg: ", round(mean_mm, 0), " mm")),
    hjust = -0.15, vjust = -0.8, size = 3.2, color = "#1EBFB3", fontface = "italic",
    inherit.aes = FALSE
  ) +
  # Would-be trigger threshold crossbar + label
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
  ) +
  # Current forecast point + label
  geom_point(
    data = \(d) filter(d, point_type == "current"),
    color = "#F2645A", size = 5, shape = 18
  ) +
  geom_text(
    data = df_current,
    aes(x = aoi_label, y = fcst_mm, label = label_left),
    hjust = 1.2, vjust = 0.4, size = 3.5, color = "#F2645A", fontface = "bold",
    inherit.aes = FALSE
  ) +
  facet_wrap(~aoi_label, nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title = glue(
      "Current & Historical Rainfall {month_label_en} Published Forecasts\n",
      "for {season_label} Season ({season_months_abbr}) — Honduras"
    ),
    subtitle = glue("Current forecast published {month_label_en} {current_year} shown in red"),
    y = "Seasonal Rainfall (mm)",
    caption = glue(
      "All forecasted {season_months_abbr} rainfall values from historical forecasts (same leadtime) ",
      "{BASELINE_START}–{BASELINE_END} shown as points.\n",
      "Teal line: historical average. Purple line: would-be trigger threshold."
    )
  ) +
  theme(
    axis.title.x  = element_blank(),
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    title         = element_text(size = 14),
    plot.subtitle = element_text(size = 13),
    strip.text    = element_text(size = 13),
    plot.caption  = element_text(hjust = 0, size = 11)
  )

print(p_hnd)

dir.create(OUT_PNG, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(
  OUT_PNG, glue("hnd_forecast_{season}_{current_year}_seas5_{tolower(month_label_en)}.png")
)
ggsave(out_file, p_hnd, width = 6.5, height = 7, dpi = 150, bg = "white")
cat("wrote", out_file, "\n")
