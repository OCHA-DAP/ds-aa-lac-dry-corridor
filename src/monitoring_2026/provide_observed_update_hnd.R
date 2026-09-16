#' Observed-Rainfall Informational Update — Honduras
#'
#' Companion to provide_informational_update.R, requested by Honduras. Same
#' visual (jitter of historical values + highlighted current value) but using
#' OBSERVED ERA5 monthly rainfall from the team DB instead of SEAS5 forecasts:
#' the latest fully observed season vs the same season in the historical record.
#'
#' Produces:
#'   1. Jitter plot: observed seasonal total for the current year vs 1991-2024
#'      distribution for the Honduras AOI (El Paraiso + Francisco Morazan),
#'      labelled with mm, % below normal, and dry rank.
#'   2. Summary stats printed to console (for email text).
#'
#' Usage: source/run interactively. Season is auto-detected as the most recent
#' season for which every month is present in ERA5; override `season` below.

library(dplyr)
library(lubridate)
library(glue)
library(ggplot2)
library(cumulus)
library(gghdx)

gghdx()


# 0. Configuration --------------------------------------------------------

BASELINE_START <- 1991   # match SEAS5 informational plot for side-by-side reading
BASELINE_END   <- 2024

SEASONS <- list(
  primera  = list(months = 5:8,  abbr = "MJJA", range_en = "May–August"),
  postrera = list(months = 9:11, abbr = "SON",  range_en = "September–November")
)

hnd_cfg <- list(
  label     = "Honduras",
  pcodes    = c("HN07", "HN08"),
  iso3      = "HND",
  admin1    = "El Paraíso, Francisco Morazán"
)

OUT_PNG <- "src/monitoring_2026/png"


# 1. Load ERA5 monthly + weights ------------------------------------------

con <- pg_con()
on.exit(DBI::dbDisconnect(con))

cat("Loading ERA5 admin-1 monthly data (HND)...\n")
# `mean` is stored as a rate (mm/day); convert to a monthly total.
df_era5 <- tbl(con, "era5") |>
  mutate(across(pcode, as.character)) |>
  filter(iso3 == !!hnd_cfg$iso3, adm_level == 1, pcode %in% !!hnd_cfg$pcodes) |>
  select(pcode, valid_date, mean) |>
  collect() |>
  mutate(
    mm    = days_in_month(valid_date) * mean,
    year  = year(valid_date),
    month = month(valid_date)
  )

if (nrow(df_era5) == 0) stop("No ERA5 rows returned for HND pcodes ", toString(hnd_cfg$pcodes))

df_weights <- tbl(con, "polygon") |>
  mutate(across(pcode, as.character)) |>
  filter(adm_level == 1, pcode %in% !!hnd_cfg$pcodes) |>
  select(pcode, era5_n_upsampled_pixels) |>
  collect()

if (nrow(df_weights) != length(hnd_cfg$pcodes) || any(is.na(df_weights$era5_n_upsampled_pixels))) {
  stop("Missing ERA5 pixel weights for one or more HND pcodes.")
}

latest_date <- max(df_era5$valid_date)
cat(glue("ERA5 available through {format(latest_date, '%B %Y')}"), "\n")


# 2. Detect latest fully observed season ----------------------------------

# A season is "fully observed" in a year if every one of its months is present
# for every pcode. Pick the (year, season) with the latest final month.
season_completeness <- bind_rows(lapply(names(SEASONS), \(s) {
  mths <- SEASONS[[s]]$months
  df_era5 |>
    filter(month %in% mths) |>
    group_by(year) |>
    summarise(
      complete = n() == length(mths) * length(hnd_cfg$pcodes),
      .groups  = "drop"
    ) |>
    mutate(season = s, end_month = max(mths))
}))

latest_season <- season_completeness |>
  filter(complete) |>
  arrange(desc(year), desc(end_month)) |>
  slice(1)

season       <- latest_season$season
current_year <- latest_season$year
# season <- "primera"; current_year <- 2026   # manual override

season_cfg    <- SEASONS[[season]]
season_label  <- tools::toTitleCase(season)
season_months <- season_cfg$months

cat(glue("Season: {season_label} ({season_cfg$abbr}) | Year: {current_year}"), "\n")


# 3. Seasonal total per pcode -> area-weighted AOI total ------------------

df_season_pcode <- df_era5 |>
  filter(month %in% season_months) |>
  group_by(pcode, year) |>
  filter(all(season_months %in% month)) |>   # drop partial seasons
  summarise(obs_mm = sum(mm), .groups = "drop")

df_aoi <- df_season_pcode |>
  left_join(df_weights, by = "pcode") |>
  group_by(year) |>
  filter(n() == length(hnd_cfg$pcodes)) |>   # need both pcodes in a year
  summarise(
    obs_mm = weighted.mean(obs_mm, w = era5_n_upsampled_pixels),
    .groups = "drop"
  ) |>
  mutate(aoi_label = hnd_cfg$label)

df_hist <- df_aoi |> filter(year >= BASELINE_START, year <= BASELINE_END)
df_curr <- df_aoi |> filter(year == current_year)

if (nrow(df_curr) != 1) stop("Expected exactly one current-year row, got ", nrow(df_curr))
if (nrow(df_hist) != BASELINE_END - BASELINE_START + 1) {
  stop("Baseline has ", nrow(df_hist), " years; expected ", BASELINE_END - BASELINE_START + 1)
}


# 4. Stats for labels -----------------------------------------------------

hist_mean <- mean(df_hist$obs_mm)
n_hist    <- nrow(df_hist)
# Rank of the current year among baseline years (1 = driest)
dry_rank  <- sum(df_hist$obs_mm < df_curr$obs_mm) + 1

df_curr <- df_curr |>
  mutate(
    hist_mean  = hist_mean,
    pct_below  = round((1 - obs_mm / hist_mean) * 100, 0),
    dry_rank   = dry_rank,
    label_left = paste0(round(obs_mm, 0), " mm")
  )

cat("\n", glue(
  "{season_label} {current_year} observed: {round(df_curr$obs_mm)} mm | ",
  "historical avg ({BASELINE_START}–{BASELINE_END}): {round(hist_mean)} mm | ",
  "{df_curr$pct_below}% below normal | ",
  "rank {dry_rank} of {n_hist + 1} driest (baseline + current)"
), "\n\n")


# 5. Plot -----------------------------------------------------------------

df_plot <- bind_rows(
  df_hist |> mutate(point_type = "hist"),
  df_curr |> select(year, obs_mm, aoi_label) |> mutate(point_type = "current")
)

p_hnd <- ggplot(df_plot, aes(x = aoi_label, y = obs_mm)) +
  geom_jitter(
    data = \(d) filter(d, point_type == "hist"),
    width = 0.15, alpha = 0.35, color = "grey55", size = 2.5
  ) +
  geom_crossbar(
    data = tibble(aoi_label = hnd_cfg$label, mean_mm = hist_mean),
    aes(x = aoi_label, y = mean_mm, ymin = mean_mm, ymax = mean_mm),
    width = 0.35, color = "#1EBFB3", linewidth = 0.6, fatten = 2,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = tibble(aoi_label = hnd_cfg$label, mean_mm = hist_mean),
    aes(x = aoi_label, y = mean_mm, label = paste0("avg: ", round(mean_mm, 0), " mm")),
    hjust = -0.15, vjust = -0.8, size = 3.2, color = "#1EBFB3", fontface = "italic",
    inherit.aes = FALSE
  ) +
  geom_point(
    data = \(d) filter(d, point_type == "current"),
    color = "#F2645A", size = 5, shape = 18
  ) +
  geom_text(
    data = df_curr,
    aes(x = aoi_label, y = obs_mm, label = label_left),
    hjust = 1.2, vjust = 0.4, size = 3.5, color = "#F2645A", fontface = "bold",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = df_curr,
    aes(x = aoi_label, y = obs_mm,
        label = glue("{pct_below}% below avg\n{scales::ordinal(dry_rank)} driest of {n_hist + 1}")),
    hjust = -0.15, vjust = 0.5, size = 3.2, color = "#F2645A", fontface = "italic",
    inherit.aes = FALSE
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title = glue("Observed Rainfall: {season_label} Season ({season_cfg$abbr}) {current_year} vs Historical — Honduras"),
    subtitle = glue("Observed {season_cfg$range_en} {current_year} rainfall (ERA5) shown in red; AOI: {hnd_cfg$admin1}"),
    y = "Seasonal Rainfall (mm)",
    caption = glue(
      "Observed {season_cfg$abbr} rainfall totals {BASELINE_START}–{BASELINE_END} shown as grey points (ERA5 monthly, area-weighted across the two departments).\n",
      "Teal line: historical average. Rank counts the current year among baseline years plus itself."
    )
  ) +
  theme(
    axis.title.x  = element_blank(),
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    title         = element_text(size = 14),
    plot.subtitle = element_text(size = 12),
    plot.caption  = element_text(hjust = 0, size = 10)
  )

print(p_hnd)

dir.create(OUT_PNG, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(OUT_PNG, glue("hnd_observed_{season}_{current_year}_era5.png"))
ggsave(out_file, p_hnd, width = 6.5, height = 7, dpi = 150, bg = "white")
cat("wrote", out_file, "\n")
