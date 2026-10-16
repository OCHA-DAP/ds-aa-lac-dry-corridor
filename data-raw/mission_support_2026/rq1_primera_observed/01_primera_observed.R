# RQ1 — Observed 2026 primera performance so far (Tier-1, observational).
#
# Primera = May-Aug; as of early July only May-Jun are observed. Compute the May-Jun 2026
# total per admin unit -> where it ranks among 1991-2024 + 2026 (MSU_RP_YEARS; percentile /
# empirical RP). Then overlay against rq4's historical El Nino anomaly: the strongest mission
# statement is "historical El Nino loser AND already dry in 2026".
#
# ERA5 now has through 2026-06 (checked 2026-07-07). This is a PARTIAL-season read; rq2
# completes the season with the SEAS5 Jul-Aug forecast (mixed blend).

source(file.path("data-raw/mission_support_2026/_shared/funcs.R"))
library(purrr)
library(sf)

out_png <- "data-raw/mission_support_2026/rq1_primera_observed/png"
out_csv <- "data-raw/mission_support_2026/rq1_primera_observed"

OBS_MONTHS <- 5:6   # May-Jun observed so far
TARGET_YEAR <- 2026

meta <- msu_admin_meta(adm_level = 1L) |> select(iso3, pcode, name)
era5 <- msu_era5_monthly(adm_level = 1L)

# May-Jun total per unit per year (all months present required)
mj <- msu_seasonal_total(era5, OBS_MONTHS) |>
  left_join(meta, by = c("iso3", "pcode"))

# Percentile + empirical dry return period of each year among MSU_RP_YEARS (1991-2024 + 2026).
# Weibull plotting position rank/(n+1): bounds the driest-on-record RP at n+1 (36 yr here),
# avoiding the tail over-precision of the Hazen position.
# pctile: low = dry (dry-side plotting position). rp_dry = 1/pctile.
mj_rank <- mj |>
  filter(year %in% MSU_RP_YEARS) |>
  group_by(iso3, pcode, name) |>
  mutate(
    n_years = n(),
    pctile = rank(mm, ties.method = "min") / (n_years + 1),
    rp_dry = 1 / pctile,
    dry_rank = rank(mm, ties.method = "min")   # 1 = driest on record
  ) |>
  ungroup()

obs_2026 <- mj_rank |>
  filter(year == TARGET_YEAR) |>
  select(iso3, pcode, name, mm_2026 = mm, pctile, rp_dry, dry_rank, n_years)

readr::write_csv(obs_2026, file.path(out_csv, "primera_observed_2026.csv"))

ranked <- obs_2026 |> arrange(dry_rank) |>
  select(iso3, name, mm_2026, dry_rank, n_years, rp_dry)
cat("\n=== Driest May-Jun 2026 admin-1 units (dry_rank 1 = driest in record) ===\n")
print(as.data.frame(head(ranked, 15)), digits = 3)
cat("\n2026 May-Jun rank distribution across 54 units (1 = driest in", obs_2026$n_years[1], "yrs):\n")
print(table(obs_2026$dry_rank))
cat("Units in their bottom tercile (pctile < 0.33):",
    sum(obs_2026$pctile < 1/3), "of", nrow(obs_2026), "\n")

# --- Combine with rq4 El Nino analog ----------------------------------------
enso_path <- "data-raw/mission_support_2026/rq4_enso_context/enso_composite_by_unit.csv"
if (file.exists(enso_path)) {
  enso_comp <- readr::read_csv(enso_path, show_col_types = FALSE) |>
    filter(window == "primera") |>
    select(pcode, enso_anom = pct_anom, enso_sig = sig10)

  combined <- obs_2026 |> left_join(enso_comp, by = "pcode")
  readr::write_csv(combined, file.path(out_csv, "primera_2026_vs_enso.csv"))

  gghdx_ok <- requireNamespace("gghdx", quietly = TRUE)
  if (gghdx_ok) gghdx::gghdx()

  # Quadrant scatter: historical El Nino sensitivity (x) vs 2026 observed percentile (y).
  # Bottom-left = historically El Nino-sensitive AND already dry in 2026 = priority.
  p_quad <- combined |>
    ggplot(aes(x = enso_anom, y = pctile, color = iso3)) +
    geom_hline(yintercept = 1/3, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_point(aes(shape = enso_sig), size = 3, alpha = 0.85) +
    ggrepel::geom_text_repel(
      data = ~filter(.x, pctile < 1/3 & enso_anom < -0.10),
      aes(label = paste(iso3, name)), size = 3, max.overlaps = 20, show.legend = FALSE) +
    scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1),
                       labels = c(`TRUE` = "El Nino signif.", `FALSE` = "n.s."), name = NULL) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    labs(title = "Priority quadrant: historical El Nino sensitivity vs 2026 primera so far",
         subtitle = "x = mean primera rainfall anomaly in past El Nino years; y = May-Jun 2026 percentile in unit's own record\nBottom-left = historically El Nino-sensitive AND already dry in 2026",
         x = "Historical El Nino primera anomaly (more negative = more sensitive)",
         y = "May-Jun 2026 percentile (lower = drier)") +
    theme(legend.position = "top")
  msu_save_png(p_quad, file.path(out_png, "02_priority_quadrant.png"), width = 10, height = 7.5)
}

# --- Map of 2026 May-Jun percentile -----------------------------------------
iso_lower <- c("gtm", "hnd", "slv")
gdf_adm1 <- iso_lower |>
  map(\(i) cumulus::download_fieldmaps_sf(iso3 = i, layer = paste0(i, "_adm1"))[[paste0(i, "_adm1")]]) |>
  map(\(g) { names(g)[names(g) == "geom"] <- "geometry"; st_geometry(g) <- "geometry"; g }) |>
  map(\(g) janitor::clean_names(g)) |>
  map(\(g) {
    pcol <- names(g)[grepl("adm1_pcode", names(g))][1]
    g |> rename(pcode = all_of(pcol)) |> select(pcode, geometry)
  }) |>
  bind_rows()

map_df <- gdf_adm1 |> left_join(obs_2026, by = "pcode") |> filter(!is.na(pctile))

base_map_theme <- theme(axis.text = element_blank(), axis.ticks = element_blank(),
                        panel.grid = element_blank())

p_map <- map_df |>
  ggplot() +
  geom_sf(aes(fill = pctile), color = "grey80", linewidth = 0.1) +
  facet_wrap(~iso3, nrow = 1) +
  scale_fill_gradient(low = "#67000d", high = "#fff5f0", labels = scales::percent,
                      limits = c(0, 1), name = "May-Jun 2026\npercentile") +
  labs(title = "2026 primera so far (May-Jun observed) by admin-1 unit",
       subtitle = "Percentile of May-Jun 2026 rainfall among 1991-2024 + 2026 in each unit. Dark red = driest.",
       caption = "ERA5, observed through June 2026. Partial season (May-Jun of May-Aug primera).") +
  base_map_theme
msu_save_png(p_map, file.path(out_png, "01_primera_2026_map.png"), width = 11, height = 4.5)

# Return-period view (frequently cited: "1-in-N year" dryness). RP = (n+1)/dry_rank, Weibull.
map_df_rp <- map_df |> mutate(rp_class = msu_rp_class(rp_dry))
p_map_rp <- map_df_rp |>
  ggplot() +
  geom_sf(aes(fill = rp_class), color = "grey80", linewidth = 0.1) +
  facet_wrap(~iso3, nrow = 1) +
  scale_fill_manual(values = MSU_RP_FILL, drop = FALSE, na.value = "grey90",
                    name = "Dry return\nperiod (years)") +
  labs(title = "2026 primera so far — dry return period by admin-1 unit",
       subtitle = "Empirical return period of the May-Jun 2026 rainfall deficit (Weibull, 1991-2024 + 2026).\nDarker = rarer/drier (e.g. ≥20 = worst in a generation).",
       caption = "ERA5, through June 2026. RP = (n+1)/rank; max 36yr for driest-on-record.") +
  base_map_theme
msu_save_png(p_map_rp, file.path(out_png, "01b_primera_2026_rp_map.png"), width = 11, height = 4.5)
