# RQ4 — El Nino historical analogs: which admin-1 units dry out under El Nino?
#
# Tier-1 targeting product. Observational (ERA5), so full sub-national resolution.
# For each adm1 unit x season, compare mean rainfall in El Nino years vs the all-year mean
# -> % anomaly. Units that dry out most under El Nino are the historical "El Nino losers",
# and 2026 is a developing El Nino -> these are the units to watch this year.

source(file.path("data-raw/mission_support_2026/_shared/funcs.R"))
library(purrr)
library(sf)

out_png <- "data-raw/mission_support_2026/rq4_enso_context/png"
out_csv <- "data-raw/mission_support_2026/rq4_enso_context"

# --- Seasonal totals per adm1 unit + ENSO labels ----------------------------
meta <- msu_admin_meta(adm_level = 1L) |> select(iso3, pcode, name)
era5 <- msu_era5_monthly(adm_level = 1L)
enso <- msu_enso_by_year(oni_months = 5:11)

seasonal <- bind_rows(
  msu_seasonal_total(era5, MSU_SEASONS$primera) |> mutate(window = "primera"),
  msu_seasonal_total(era5, MSU_SEASONS$postrera) |> mutate(window = "postrera")
) |>
  left_join(meta, by = c("iso3", "pcode")) |>
  left_join(enso, by = "year")

# --- El Nino composite anomaly per unit x season ----------------------------
composite <- seasonal |>
  group_by(iso3, pcode, name, window) |>
  summarise(
    n_all = n(),
    n_elnino = sum(enso == "El Nino"),
    mean_all = mean(mm),
    mean_elnino = mean(mm[enso == "El Nino"]),
    median_all = median(mm),
    median_elnino = median(mm[enso == "El Nino"]),
    # failure freq in El Nino years using each unit's own all-year bottom-tercile cutoff
    fail_cut = quantile(mm, 1/3),
    fail_freq_elnino = mean(mm[enso == "El Nino"] <= fail_cut),
    .groups = "drop"
  ) |>
  mutate(
    pct_anom = mean_elnino / mean_all - 1,       # negative => drier under El Nino
    pct_anom_median = median_elnino / median_all - 1
  )

# --- Significance: is each unit's El Nino dryness distinguishable from noise? ---
# n El Nino ~ 10, so a raw % anomaly can be luck. One-sided Wilcoxon (El Nino totals lower
# than non-El Nino) per unit x season. Rank-based => robust to skewed rainfall totals.
sig <- seasonal |>
  group_by(iso3, pcode, name, window) |>
  summarise(
    p_wilcox = tryCatch(
      wilcox.test(mm[enso == "El Nino"], mm[enso != "El Nino"],
                  alternative = "less")$p.value,
      error = function(e) NA_real_
    ),
    .groups = "drop"
  )

composite <- composite |>
  left_join(sig, by = c("iso3", "pcode", "name", "window")) |>
  mutate(sig10 = !is.na(p_wilcox) & p_wilcox < 0.10)

readr::write_csv(composite, file.path(out_csv, "enso_composite_by_unit.csv"))

# Ranked "El Nino losers" (driest units under El Nino, primera)
ranked_primera <- composite |>
  filter(window == "primera") |>
  arrange(pct_anom) |>
  select(iso3, name, mean_all, mean_elnino, pct_anom, fail_freq_elnino, p_wilcox, sig10)

cat("\n=== Driest admin-1 under El Nino, PRIMERA (top 12) ===\n")
print(as.data.frame(head(ranked_primera, 12)), digits = 3)
cat("\nUnits with El Nino primera dryness significant at p<0.10:",
    sum(ranked_primera$sig10), "of", nrow(ranked_primera), "\n")

ranked_postrera <- composite |>
  filter(window == "postrera") |>
  arrange(pct_anom) |>
  select(iso3, name, mean_all, mean_elnino, pct_anom, fail_freq_elnino, n_elnino)
cat("\n=== Driest admin-1 under El Nino, POSTRERA (top 12) ===\n")
print(as.data.frame(head(ranked_postrera, 12)), digits = 3)

# --- Boundaries for mapping -------------------------------------------------
iso_lower <- c("gtm", "hnd", "slv")
gdf_adm1 <- iso_lower |>
  map(\(i) cumulus::download_fieldmaps_sf(iso3 = i, layer = paste0(i, "_adm1"))[[paste0(i, "_adm1")]]) |>
  map(\(g) { names(g)[names(g) == "geom"] <- "geometry"; st_geometry(g) <- "geometry"; g }) |>
  map(\(g) janitor::clean_names(g))

# find the pcode column per layer and standardise
gdf_adm1 <- gdf_adm1 |>
  map(\(g) {
    pcol <- names(g)[grepl("adm1_pcode", names(g))][1]
    g |> rename(pcode = all_of(pcol)) |> select(pcode, geometry)
  }) |>
  bind_rows()

map_df <- gdf_adm1 |>
  left_join(composite, by = "pcode") |>
  filter(!is.na(window))

gghdx_ok <- requireNamespace("gghdx", quietly = TRUE)
if (gghdx_ok) gghdx::gghdx()

# Sequential red scale: every unit is drier under El Nino, so a diverging scale wastes half
# its range. More negative = deeper red. Hatch/outline the statistically significant units.
p_map <- map_df |>
  ggplot() +
  geom_sf(aes(fill = pct_anom), color = "grey80", linewidth = 0.1) +
  geom_sf(data = filter(map_df, sig10), aes(geometry = geometry),
          fill = NA, color = "black", linewidth = 0.45) +
  facet_grid(window ~ iso3, switch = "y") +
  scale_fill_gradient(low = "#67000d", high = "#fff5f0", labels = scales::percent,
                      name = "El Nino rainfall\nanomaly vs normal") +
  labs(title = "Historical El Nino rainfall anomaly by admin-1 unit",
       subtitle = "Mean seasonal rainfall in El Nino years vs all-year mean (ERA5, 1981-2025)\nDarker red = drier under El Nino. Black outline = significant (Wilcoxon p<0.10).",
       caption = "El Nino years by MJJ-SON ONI (~10 years).") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank())
msu_save_png(p_map, file.path(out_png, "01_enso_anomaly_map.png"), width = 11, height = 7)

# Ranked dot plot (primera) — top 15 driest
p_rank <- ranked_primera |>
  slice_head(n = 15) |>
  mutate(unit = paste(iso3, name)) |>
  ggplot(aes(x = pct_anom, y = reorder(unit, -pct_anom), color = iso3)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(shape = sig10), size = 3) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1),
                     labels = c(`TRUE` = "p<0.10", `FALSE` = "n.s."), name = NULL) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Where primera dries out most under El Nino (top 15 admin-1 units)",
       subtitle = "Mean primera rainfall in El Nino years relative to normal (ERA5, 1981-2025)\nFilled = statistically significant; hollow = not distinguishable from noise",
       x = "El Nino primera anomaly", y = NULL) +
  theme(legend.position = "top")
msu_save_png(p_rank, file.path(out_png, "02_primera_elnino_ranking.png"), width = 9, height = 7)
