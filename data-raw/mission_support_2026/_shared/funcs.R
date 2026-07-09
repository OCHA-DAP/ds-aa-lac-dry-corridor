# Shared helpers for the ROLAC mission-support scratch analyses.
# Plain sourceable functions (not box modules) to keep the scratch tree simple.
# Reuses patterns from R/seas5.R and exploration/13_refine_aoi.R.

suppressMessages({
  library(dplyr)
  library(lubridate)
  library(tidyr)
  library(stringr)
  library(ggplot2)
})

MSU_ISO3 <- c("GTM", "HND", "SLV")
MSU_SEASONS <- list(primera = 5:8, postrera = 9:11)

msu_con <- function() cumulus::pg_con()

# Admin metadata + human-readable names + per-sensor pixel weights, one row per pcode.
msu_admin_meta <- function(iso3s = MSU_ISO3, adm_level = 1L, con = msu_con()) {
  meta <- tbl(con, "polygon") |>
    filter(iso3 %in% iso3s, adm_level == !!adm_level) |>
    collect() |>
    janitor::clean_names()
  meta
}

# Pull ERA5 monthly and convert the stored rate (mean, mm/day) to a monthly total (mm).
msu_era5_monthly <- function(iso3s = MSU_ISO3, adm_level = 1L, con = msu_con()) {
  tbl(con, "era5") |>
    filter(iso3 %in% iso3s, adm_level == !!adm_level) |>
    select(iso3, pcode, adm_level, valid_date, mean) |>
    collect() |>
    mutate(mm = days_in_month(valid_date) * mean,
           year = year(valid_date),
           month = month(valid_date))
}

# Aggregate monthly mm to a seasonal total per (pcode, year).
# Requires every month of the season to be present, else the year is dropped (partial season).
msu_seasonal_total <- function(df_monthly, months, by = c("iso3", "pcode", "year")) {
  df_monthly |>
    filter(month %in% months) |>
    group_by(across(all_of(by))) |>
    filter(all(months %in% month)) |>
    summarise(mm = sum(mm), n_months = n(), .groups = "drop")
}

# Empirical return period per group. Replicates the repo's Weibull plotting position
# (src/utils/gen_utils.R::threshold_var): q_rank = rank/(n+1), rp_emp = 1/q_rank.
# direction = -1 => low value = high RP (drought). Driest-on-record => rp_emp = n+1.
msu_emp_rp <- function(df, var = "mm", by = c("pcode"), direction = -1) {
  df |>
    group_by(across(all_of(by))) |>
    mutate(
      rank = if (direction == -1) rank(.data[[var]], ties.method = "first")
             else rank(-.data[[var]], ties.method = "first"),
      q_rank = rank / (max(rank) + 1),
      rp_emp = 1 / q_rank,
      pctile = q_rank
    ) |>
    ungroup() |>
    select(-rank, -q_rank)
}

# Bin an empirical RP into the drought RP classes we cite in reporting (1-in-N years).
MSU_RP_BREAKS <- c(0, 2, 3, 5, 10, 20, Inf)
MSU_RP_LABELS <- c("<2", "2–3", "3–5", "5–10", "10–20", "≥20")
msu_rp_class <- function(rp) {
  factor(cut(rp, breaks = MSU_RP_BREAKS, labels = MSU_RP_LABELS, right = FALSE),
         levels = MSU_RP_LABELS)
}
# Sequential reds for RP classes (rarer/drier = darker). Named for scale_fill_manual.
MSU_RP_FILL <- setNames(
  c("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15", "#67000d"),
  MSU_RP_LABELS
)

# ENSO phase per calendar year from ONI. Uses the mean ONI anomaly over a chosen
# month window; classifies El Nino / La Nina / Neutral with the conventional +/-0.5 threshold.
msu_enso_by_year <- function(oni_months = 5:11, warm = 0.5, cool = -0.5) {
  oni <- cumulus::load_oni()
  oni |>
    filter(mon %in% oni_months) |>
    group_by(yr) |>
    summarise(oni = mean(anom, na.rm = TRUE), .groups = "drop") |>
    mutate(enso = case_when(
      oni >= warm ~ "El Nino",
      oni <= cool ~ "La Nina",
      TRUE ~ "Neutral"
    )) |>
    rename(year = yr)
}

msu_save_png <- function(plot, path, width = 9, height = 6, dpi = 150) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)  # ggsave won't create it
  ggsave(path, plot, width = width, height = height, dpi = dpi, bg = "white")
  message("wrote ", path)
}

# Robust admin-name key for joining across data sources (accents, case, punctuation, articles).
msu_name_key <- function(x) {
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(trimws(x))
  x <- gsub("^(el|la|los|las|de|del)\\s+", "", x)   # drop leading articles (El Paraiso -> paraiso)
  gsub("[^a-z0-9]", "", x)
}

# Dependency-free diagonal hatching: return LINESTRINGs (45 deg) clipped to `poly_sf`,
# for drawing a "striped / not analysed" fill without ggpattern.
msu_hatch <- function(poly_sf, n = 80) {
  suppressWarnings({
    u <- sf::st_union(sf::st_geometry(poly_sf))
    bb <- sf::st_bbox(u)
    cs <- seq((bb["ymin"] - bb["xmax"]), (bb["ymax"] - bb["xmin"]), length.out = n)  # y = x + c
    lines <- lapply(cs, function(cc) {
      sf::st_linestring(matrix(c(bb["xmin"], bb["xmin"] + cc,
                                 bb["xmax"], bb["xmax"] + cc), ncol = 2, byrow = TRUE))
    })
    grid <- sf::st_sfc(lines, crs = sf::st_crs(poly_sf))
    sf::st_intersection(grid, u)
  })
}
