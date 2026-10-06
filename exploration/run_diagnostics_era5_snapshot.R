# Trigger Optimization Diagnostics
#
# Runs the full grid-search -> evaluate -> filter -> rank pipeline for
# HND, GTM, SLV and generates diagnostic plots and tables for each.
#
# Run section-by-section per country to inspect results interactively.
# Outputs saved to artefacts/trigger_optimization/{country}/

library(tidyverse)
library(lubridate)
library(cumulus)
library(gt)
library(janitor)

box::purge_cache()
box::use(
  ../../R/country_trigger_optimization[...],
  ../../R/trigger_viz[...]
)
df_cerf <- cumulus::load_cerf_from_blob() |> clean_names()
df_emdat <- cumulus::load_emdat_from_blob() |> clean_names()

# ═══════════════════════════════════════════════════════════════════════════════
# Impact data
# ═══════════════════════════════════════════════════════════════════════════════

iso_lookup <- c(HND = "Honduras", GTM = "Guatemala", SLV = "El Salvador")

df_cerf_impact <- df_cerf |>
  filter(
    country_code %in% names(iso_lookup),
    window_full_name == "Rapid Response",
    emergency_type_name == "Drought"
  ) |>
  mutate(year = year(date_usg_signature)) |>
  distinct(iso3 = country_code, year) |>
  mutate(dataset = "CERF")

# CERF Hypothesis:
# 2016 is a late drought response, allocations in 2016 can be associated
# with 2 year drought in 2014-2015.
# 2019 is a late response and can be associated 2018 drought.
# ignore 2025 as an outlier

cerf_start_drought_years <- c("2018","2015","2014","2009")

# approximate CERF RP for the region (across all region - any region)
(length(2006:2024)+1)/4


# qucik look at EM-DAT severity data:


# EMDAT Hypothesis: 

# Let's use 2 baselines: 
# a.) 1991-2024  - longer record can compare w/ seas5 eventually
# b.) 2006-2024 - shorter record only to directly compare w/ CERF

df_emdat_filt <- df_emdat |>
  filter(
    iso %in% names(iso_lookup),
    disaster_type == "Drought"
  ) 

df_emdat_filt |> 
  ggplot(
    aes(x= start_year, y= total_affected)
  )+
  geom_bar(stat= "identity")+ 
  coord_flip()+
  facet_wrap(~iso)

df_emdat_impact <- df_emdat_filt |> 
  distinct(iso3 = iso, year = start_year) |>
  mutate(dataset = "EM-DAT")

df_emdat_distinct_years <- df_emdat_impact |> 
  distinct(year)

n_years_drough_emdat_b91<- df_emdat_distinct_years |> 
  filter(
    year>=1991, year<=2024
  ) |> 
  pull(year) |> 
  length()

n_years_drough_emdat_b06<- df_emdat_distinct_years |> 
  filter(
    year>=2006, year<=2024
  ) |> 
  pull(year) |> 
  length()

# RP EM-DAT: REGIONAL UNION (any country in region has event)
# 2006 baseline
(length(2006:2024)+1)/n_years_drough_emdat_b06
# 1991 baseline
(length(1991:2024)+1)/n_years_drough_emdat_b91

# RP EM-DAT: PER COUNTRY
emdat_rp_by_country <- df_emdat_impact |>
  filter(year >= 1991, year <= 2024) |>
  group_by(iso3) |>
  summarise(
    n_event_years = n_distinct(year),
    rp_1991 = (length(1991:2024) + 1) / n_distinct(year),
    .groups = "drop"
  ) |>
  mutate(
    n_event_years_06 = map_int(iso3, \(x) {
      df_emdat_impact |>
        filter(iso3 == x, year >= 2006, year <= 2024) |>
        n_distinct()
    }),
    rp_2006 = (length(2006:2024) + 1) / n_event_years_06
  )
emdat_rp_by_country

df_impact <- bind_rows(df_cerf_impact, df_emdat_impact) |>
  mutate(country = iso_lookup[iso3])

# Heatmap: impact events by year, dataset, and country
year_range <- range(df_impact$year)
impact_grid <- expand_grid(
  iso3 = names(iso_lookup),
  year = seq(year_range[1], year_range[2]),
  dataset = c("CERF", "EM-DAT")
) |>
  mutate(country = iso_lookup[iso3]) |>
  left_join(
    df_impact |> mutate(event = TRUE),
    by = c("iso3", "year", "dataset", "country")
  ) |>
  mutate(event = replace_na(event, FALSE))

ggplot(impact_grid, aes(x = dataset, y = factor(year), fill = event)) +
  geom_tile(color = "grey80", linewidth = 0.3) +
  scale_fill_manual(
    values = c("TRUE" = "#d62728", "FALSE" = "grey95"),
    labels = c("TRUE" = "Event", "FALSE" = "No event"),
    name = NULL
  ) +
  facet_wrap(~country) +
  labs(x = NULL, y = NULL, title = "Drought Impact Events: CERF vs EM-DAT") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom"
  )


# ═══════════════════════════════════════════════════════════════════════════════
# Country configurations
# ═══════════════════════════════════════════════════════════════════════════════

countries <- list(
  HND = list(
    aoi_pcodes = c("HN07", "HN08"),
    primera_months = 5:8,
    postrera_months = 9:11,
    primera_issued_months = c(3, 4, 5),
    postrera_issued_months = c(6, 7, 8, 9)
  ),
  GTM = list(
    aoi_pcodes = c("GT20"),
    primera_months = 5:8,
    postrera_months = 9:11,
    primera_issued_months = c(3, 4, 5),
    postrera_issued_months = c(6, 7, 8, 9)
  ),
  SLV = list(
    aoi_pcodes = c("SV01", "SV12"),
    primera_months = 5:8,
    postrera_months = 9:11,
    primera_issued_months = c(3, 4, 5),
    postrera_issued_months = c(6, 7, 8, 9)
  )
)

BASELINE_START <- 1981
BASELINE_END <- 2024

# ═══════════════════════════════════════════════════════════════════════════════
# Data loading
# ═══════════════════════════════════════════════════════════════════════════════

load_country_data <- function(cfg) {
  con <- pg_con()
  on.exit(DBI::dbDisconnect(con))

  df_weights <- tbl(con, "polygon") |>
    mutate(across(pcode, as.character)) |>
    filter(adm_level == 1, pcode %in% cfg$aoi_pcodes) |>
    select(pcode, seas5_n_upsampled_pixels) |>
    collect()

  df_seas5_raw <- tbl(con, "seas5") |>
    mutate(across(pcode, as.character)) |>
    filter(adm_level == 1, pcode %in% cfg$aoi_pcodes) |>
    collect()

  df_era5_raw <- tbl(con, "era5") |>
    mutate(across(pcode, as.character)) |>
    filter(pcode %in% cfg$aoi_pcodes) |>
    collect()

  df_seas5_mm <- df_seas5_raw |>
    mutate(value_mm = days_in_month(valid_date) * mean)

  df_seas5_seasonal <- bind_rows(
    seas5_aggregate_forecast(
      df_seas5_mm, value = "value_mm",
      valid_months = cfg$primera_months,
      by = c("iso3", "pcode", "issued_date")
    ) |> mutate(window = "primera"),
    seas5_aggregate_forecast(
      df_seas5_mm, value = "value_mm",
      valid_months = cfg$postrera_months,
      by = c("iso3", "pcode", "issued_date")
    ) |> mutate(window = "postrera")
  ) |>
    rename(fcst_mm = value_mm) |>
    mutate(year = year(issued_date), issued_month = month(issued_date)) |>
    filter(
      (window == "primera" & issued_month %in% cfg$primera_issued_months) |
      (window == "postrera" & issued_month %in% cfg$postrera_issued_months)
    )

  df_era5_monthly <- df_era5_raw |>
    mutate(year = year(valid_date), month = month(valid_date),
           value_mm = mean * days_in_month(valid_date))

  df_era5_seasonal <- bind_rows(
    df_era5_monthly |> filter(month %in% cfg$primera_months) |>
      group_by(pcode, iso3, year) |>
      summarise(obs_mm = sum(value_mm), .groups = "drop") |>
      mutate(window = "primera"),
    df_era5_monthly |> filter(month %in% cfg$postrera_months) |>
      group_by(pcode, iso3, year) |>
      summarise(obs_mm = sum(value_mm), .groups = "drop") |>
      mutate(window = "postrera")
  )

  df_joined <- df_seas5_seasonal |>
    left_join(df_era5_seasonal |> select(pcode, year, window, obs_mm),
              by = c("pcode", "year", "window")) |>
    filter(!is.na(obs_mm)) |>
    left_join(df_weights, by = "pcode") |>
    group_by(year, window, leadtime) |>
    summarise(
      fcst_mm = weighted.mean(fcst_mm, w = seas5_n_upsampled_pixels),
      obs_mm  = weighted.mean(obs_mm,  w = seas5_n_upsampled_pixels),
      .groups = "drop"
    ) |>
    filter(year >= BASELINE_START, year <= BASELINE_END)

  # Reindex leadtimes to start at 0 per window
  df_joined <- df_joined |>
    group_by(window) |>
    mutate(leadtime = leadtime - min(leadtime)) |>
    ungroup()

  df_joined
}



# ═══════════════════════════════════════════════════════════════════════════════
# HONDURAS
# ═══════════════════════════════════════════════════════════════════════════════

cfg_hnd <- countries$HND
df_hnd <- load_country_data(cfg_hnd)
n_years_hnd <- length(unique(df_hnd$year))
primera_lts_hnd <- length(cfg_hnd$primera_issued_months)
postrera_lts_hnd <- length(cfg_hnd$postrera_issued_months)
lt_labels_hnd <- make_lt_labels(cfg_hnd$primera_issued_months, cfg_hnd$postrera_issued_months)

df_hnd |> count(window, leadtime)

# Grid search
combos_hnd <- generate_combos(n_years_hnd,
                               primera_lts = primera_lts_hnd,
                               postrera_lts = postrera_lts_hnd) |>
  grid_constrain_season_monotonic() |>
  grid_constrain_inter_season_monotonic() |>
  grid_constrain_lt_rp_range(min_rp = 4, max_rp = 15)

# Trigger lookup + evaluate
trigger_data_hnd <- build_trigger_lookup(df_hnd, min_rp = 2.8, max_rp = 15,
                                          primera_lts = primera_lts_hnd,
                                          postrera_lts = postrera_lts_hnd)

all_results_hnd <- evaluate_configs(combos_hnd, trigger_data_hnd)

# Pareto overview
target_rp_hnd <- n_years_hnd / 14
all_results_hnd |>
  add_lt_set() |>
  plot_pareto_f1_rp(target_rp = target_rp_hnd, closest_n_target = 5)

# ── Quick recommendation ─────────────────────────────────────────────────────
hnd_rec <- all_results_hnd |>
  add_lt_set() |>
  recommend_config(target_rp = target_rp_hnd, p_lt = 3, s_lt = 2)

hnd_rec <- all_results_hnd |>
  add_lt_set() |>
  recommend_config(target_rp = target_rp_hnd, p_lt = 3, s_lt = 3)
cat(hnd_rec$rationale)

# ── Define archetypes (detailed exploration below) ─────────────────────────── ─────────────────────────────────────────────────────────

hnd_option_a <- all_results_hnd |>
  add_lt_set() |>
  filter(p_lt_count == 3, s_lt_count == 2) |>
  filter_closest_annual_rp(target = target_rp_hnd, n = 3) |>
  filter_rp_balance(max_diff = 1) |>
  filter_top_f1() |>
  mutate(option = "A: Relaxed (RP \u00b1 1 step)")

hnd_option_b <- all_results_hnd |>
  add_lt_set() |>
  filter(p_lt_count == 3, s_lt_count == 2) |>
  filter_closest_annual_rp(target = target_rp_hnd, n = 1) |>
  filter_rp_balance(max_diff = 1) |>
  filter_top_f1() |>
  mutate(option = "B: Moderate (target RP)")

hnd_option_c <- all_results_hnd |>
  add_lt_set() |>
  filter(p_lt_count == 3, s_lt_count == 2) |>
  filter_closest_annual_rp(target = target_rp_hnd, n = 1) |>
  filter_rp_balance(max_diff = 0) |>
  filter_top_f1() |>
  mutate(option = "C: Strict (equal seasonal)")

# Option D: flat thresholds — same RP at every LT within each season (CV = 0)
hnd_option_d <- all_results_hnd |>
  add_lt_set() |>
  filter(p_lt_count == 3, s_lt_count == 2) |>
  filter(mean_cv == 0) |> 
  filter_closest_annual_rp(target = target_rp_hnd, n = 3) |>
  # you cannot keep them the balanced and get an annual rp of 3.14
  # and keep the seasonal_rps within 1
  filter_rp_balance(max_diff = 2) |>
  filter_top_f1() |>
  mutate(option = "D: Flat (uniform LT thresholds)")


# ── Step 1: Compare archetypes ────────────────────────────────────────────────
# Each archetype shares identical F1/RP — compare at that level to pick one

hnd_archetypes <- summarise_archetypes(
  bind_rows(hnd_option_a, hnd_option_b, hnd_option_c, hnd_option_d),
  n_years = trigger_data_hnd$n_years,
  trigger_data = trigger_data_hnd
)
hnd_archetypes$archetypes |> count(option,archetype_id)
hnd_archetypes$table
hnd_archetypes$plot

# Cross-archetype year comparison: which years differ across archetypes?
compare_archetypes_timing(hnd_archetypes$representatives, trigger_data_hnd, df_hnd)


# ── Step 2: Drill into chosen archetype ───────────────────────────────────────
# Pick archetype_id from the table above, then compare configs within it

hnd_archetypes$archetypes |> count(archetype_id)
hnd_chosen <- hnd_archetypes$archetypes |> filter(archetype_id == 1)  # <-- adjust
hnd_chosen |> 
  arrange(desc(early_mfp_ratio))

ggplot(
  hnd_chosen, aes(x=early_mfp_ratio, y= mean_cv )
) + geom_point()

plot_option_summary(hnd_chosen, trigger_data_hnd)$table
plot_option_summary(hnd_chosen |> 
                      arrange(desc(early_mfp_ratio)), trigger_data_hnd)$table
plot_option_summary(hnd_chosen, trigger_data_hnd)$plot
compare_configs_timing2(hnd_chosen, trigger_data_hnd, df_hnd)


# ── Step 3: Pick final config ─────────────────────────────────────────────────
# Set config_id based on what you see above

hnd_final <- hnd_chosen |> filter(config_id == hnd_chosen$config_id[1])  # <-- adjust
plot_config_detail(hnd_final, trigger_data_hnd, df_hnd, lt_labels_hnd)


# ═══════════════════════════════════════════════════════════════════════════════
# GUATEMALA
# ═══════════════════════════════════════════════════════════════════════════════

cfg_gtm <- countries$GTM
df_gtm <- load_country_data(cfg_gtm)
n_years_gtm <- length(unique(df_gtm$year))
primera_lts_gtm <- length(cfg_gtm$primera_issued_months)
postrera_lts_gtm <- length(cfg_gtm$postrera_issued_months)
lt_labels_gtm <- make_lt_labels(cfg_gtm$primera_issued_months, cfg_gtm$postrera_issued_months)

df_gtm |> count(window, leadtime)

# Grid search
combos_gtm <- generate_combos(n_years_gtm,
                               primera_lts = primera_lts_gtm,
                               postrera_lts = postrera_lts_gtm) |>
  grid_constrain_season_monotonic() |>
  grid_constrain_inter_season_monotonic() |>
  grid_constrain_lt_rp_range(min_rp = 4, max_rp = 15)

# Trigger lookup + evaluate
trigger_data_gtm <- build_trigger_lookup(df_gtm, min_rp = 2.8, max_rp = 15,
                                          primera_lts = primera_lts_gtm,
                                          postrera_lts = postrera_lts_gtm)

all_results_gtm <- evaluate_configs(combos_gtm, trigger_data_gtm)

# Pareto overview
target_rp_gtm <- n_years_gtm / 14
all_results_gtm |>
  add_lt_set() |>
  plot_pareto_f1_rp(target_rp = target_rp_gtm, closest_n_target = 5)

# ── Quick recommendation ─────────────────────────────────────────────────────
gtm_rec <- all_results_gtm |>
  add_lt_set() |>
  recommend_config(target_rp = target_rp_gtm, p_lt = 3, s_lt = 4)
cat(gtm_rec$rationale)

# ── Define archetypes (detailed exploration below) ───────────────────────────

gtm_option_a <- all_results_gtm |>
  add_lt_set() |>
  filter(p_lt_count == 3, s_lt_count == 4) |>
  filter_closest_annual_rp(target = target_rp_gtm, n = 3) |>
  filter_rp_balance(max_diff = 1) |>
  filter_top_f1() |>
  mutate(option = "A: Relaxed (RP \u00b1 1 step)")

gtm_option_b <- all_results_gtm |>
  add_lt_set() |>
  filter(p_lt_count == 3, s_lt_count == 4) |>
  filter_closest_annual_rp(target = target_rp_gtm, n = 1) |>
  filter_rp_balance(max_diff = 1) |>
  filter_top_f1() |>
  mutate(option = "B: Moderate (target RP)")

gtm_option_c <- all_results_gtm |>
  add_lt_set() |>
  filter(p_lt_count == 3, s_lt_count == 4) |>
  filter_closest_annual_rp(target = target_rp_gtm, n = 1) |>
  filter_rp_balance(max_diff = 0) |>
  filter_top_f1() |>
  mutate(option = "C: Strict (equal seasonal)")

# Option D: flat thresholds — same RP at every LT within each season (CV = 0)
gtm_option_d <- all_results_gtm |>
  add_lt_set() |>
  filter(p_lt_count == 3, s_lt_count == 4) |>
  filter(mean_cv == 0) |>
  filter_closest_annual_rp(target = target_rp_gtm, n = 3) |>
  filter_rp_balance(max_diff = 1) |>
  filter_top_f1() |>
  mutate(option = "D: Flat (uniform LT thresholds)")


# ── Step 1: Compare archetypes ────────────────────────────────────────────────

gtm_archetypes <- summarise_archetypes(
  bind_rows(gtm_option_a, gtm_option_b, gtm_option_c, gtm_option_d),
  n_years = trigger_data_gtm$n_years,
  trigger_data = trigger_data_gtm
)
gtm_archetypes$table
gtm_archetypes$plot

# Cross-archetype year comparison: which years differ across archetypes?
compare_archetypes_timing(gtm_archetypes$representatives, trigger_data_gtm, df_gtm)


# ── Step 2: Drill into chosen archetype ───────────────────────────────────────

gtm_chosen <- gtm_archetypes$archetypes |> filter(archetype_id == 1)  # <-- adjust
plot_option_summary(gtm_chosen, trigger_data_gtm)$table
plot_option_summary(gtm_chosen, trigger_data_gtm)$plot
compare_configs_timing2(gtm_chosen, trigger_data_gtm, df_gtm)


# ── Step 3: Pick final config ─────────────────────────────────────────────────

gtm_final <- gtm_chosen |> filter(config_id == gtm_chosen$config_id[1])  # <-- adjust
plot_config_detail(gtm_final, trigger_data_gtm, df_gtm, lt_labels_gtm)


# ═══════════════════════════════════════════════════════════════════════════════
# EL SALVADOR
# ═══════════════════════════════════════════════════════════════════════════════

cfg_slv <- countries$SLV
df_slv <- load_country_data(cfg_slv)
n_years_slv <- length(unique(df_slv$year))
primera_lts_slv <- 3#length(cfg_slv$primera_issued_months)
postrera_lts_slv <- 3#length(cfg_slv$postrera_issued_months)
lt_labels_slv <- make_lt_labels(cfg_slv$primera_issued_months, cfg_slv$postrera_issued_months)

df_slv |> count(window, leadtime)

# Grid search
combos_slv <- generate_combos(n_years_slv,
                               primera_lts = 3,
                               postrera_lts = 3,min_rp = 2.8,max_rp = 15) |>
  grid_constrain_season_monotonic() |>
  grid_constrain_inter_season_monotonic() |>
  grid_constrain_lt_rp_range(min_rp = 4, max_rp = 15)

  # Trigger lookup + evaluate
trigger_data_slv <- build_trigger_lookup(df_slv, min_rp = 2.8, max_rp = 15,
                                          primera_lts = primera_lts_slv,
                                          postrera_lts = postrera_lts_slv)

all_results_slv <- evaluate_configs(combos_slv, trigger_data_slv)

# Pareto overview
target_rp_slv <- n_years_slv / 14
all_results_slv |>
  add_lt_set() |>
  filter(p_lt_count == 3, s_lt_count >= 2) |>
  plot_pareto_f1_rp(target_rp = target_rp_slv, closest_n_target = 5)

# ── Quick recommendation ─────────────────────────────────────────────────────
slv_rec <- all_results_slv |>
  add_lt_set() |>
  recommend_config(target_rp = target_rp_slv, p_lt = 3, s_lt = 2)
slv_rec <- all_results_slv |>
  add_lt_set() |>
  recommend_config(target_rp = target_rp_slv, p_lt = 3, s_lt = 3)
cat(slv_rec$rationale)

# ── Define archetypes (detailed exploration below) ─────────────────────────── ─────────────────────────────────────────────────────────

slv_option_a <- all_results_slv |>
  add_lt_set() |>
  filter(p_lt_count == 3, s_lt_count == 2) |>
  filter_closest_annual_rp(target = target_rp_slv, n = 3) |>
  filter_rp_balance(max_diff = 1) |>
  filter_top_f1() |>
  mutate(option = "A: Relaxed (RP \u00b1 1 step)")

slv_option_b <- all_results_slv |>
  add_lt_set() |>
  filter(p_lt_count == 3, s_lt_count == 2) |>
  filter_closest_annual_rp(target = target_rp_slv, n = 1) |>
  filter_rp_balance(max_diff = 1) |>
  filter_top_f1() |>
  mutate(option = "B: Moderate (target RP)")

slv_option_c <- all_results_slv |>
  add_lt_set() |>
  filter(p_lt_count == 3, s_lt_count == 2) |>
  filter_closest_annual_rp(target = target_rp_slv, n = 1) |>
  filter_rp_balance(max_diff = 0) |>
  filter_top_f1() |>
  mutate(option = "C: Strict (equal seasonal)")

# Option D: flat thresholds — same RP at every LT within each season (CV = 0)
# This is the simplest possible trigger: one threshold per season
slv_option_d <- all_results_slv |>
  add_lt_set() |>
  filter(p_lt_count == 3, s_lt_count == 2) |>
  filter(mean_cv == 0) |>
  filter_closest_annual_rp(target = target_rp_slv, n = 3) |>
  filter_rp_balance(max_diff = 1) |>
  filter_top_f1() |>
  mutate(option = "D: Flat (uniform LT thresholds)")


# ── Step 1: Compare archetypes ────────────────────────────────────────────────
# Each archetype shares identical F1/RP — compare at that level to pick one
# Note: even option A (relaxed) may converge on balanced seasonal RPs

slv_archetypes <- summarise_archetypes(
  bind_rows(slv_option_a, slv_option_b, slv_option_c, slv_option_d),
  n_years = trigger_data_slv$n_years,
  trigger_data = trigger_data_slv
)
slv_archetypes$table
slv_archetypes$plot

# Cross-archetype year comparison: which years differ across archetypes?
compare_archetypes_timing(slv_archetypes$representatives, trigger_data_slv, df_slv)


# ── Step 2: Drill into chosen archetype ───────────────────────────────────────
# Pick archetype_id from the table above, then compare configs within it

slv_chosen <- slv_archetypes$archetypes |> filter(archetype_id == 1)  # <-- adjust
plot_option_summary(slv_chosen, trigger_data_slv)$table
plot_option_summary(slv_chosen, trigger_data_slv)$plot
compare_configs_timing2(slv_chosen, trigger_data_slv, df_slv)


# ── Step 3: Pick final config ─────────────────────────────────────────────────
# Set config_id based on what you see above

slv_final <- slv_chosen |> filter(config_id == slv_chosen$config_id[1])  # <-- adjust
plot_config_detail(slv_final, trigger_data_slv, df_slv, lt_labels_slv)

