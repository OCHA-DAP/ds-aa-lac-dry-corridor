# RQ2 — Mixed observational/forecast primera finalization (complete May-Aug primera estimate).
#
# Method (ADR-0002, ported from ds-aa-afg-drought): blend on Z-SCORES, not mm.
#   observed  = ERA5 May+Jun total (standardised vs its own climatology)
#   forecast  = SEAS5 Jul+Aug total from the JULY issuance (Jul=lt0, Aug=lt1; std vs SEAS5 hindcast)
#   blended z = mean(z_obs, z_fcst)   [per-block, equal weight]
#   rank 2026 blended z among the historical blended series (built the SAME mixed way each year)
# Baseline for mu/sigma: 1991-2024 (framework-aligned). Resolution (ADR-0001): adm1 GTM/HND,
# adm0 SLV, no adm2 (blend contains a SEAS5 component).

source(file.path("data-raw/mission_support_2026/_shared/funcs.R"))
library(purrr); library(sf)

out_png <- "data-raw/mission_support_2026/rq2_primera_mixed/png"
out_csv <- "data-raw/mission_support_2026/rq2_primera_mixed"
BASE <- 1991:2024
con <- msu_con()
gghdx_ok <- requireNamespace("gghdx", quietly = TRUE); if (gghdx_ok) gghdx::gghdx()

# adm1 for all three: the mixed primera is observation-DOMINATED (2 obs + 2 forecast months) and
# the observed half (ERA5 May-Jun) resolves SLV adm1 fine. SLV's *forecast* half (SEAS5 Jul-Aug)
# is coarse (~36km, sub-pixel for SLV adm1) -> within-SLV forecast detail is indicative only.
# This lets the complete-primera map parallel the observed adm1 map (Zack's "best view").
LEVELS <- list(GTM = 1L, HND = 1L, SLV = 1L)

# ---- Observed: ERA5 May+Jun total per unit-year ----------------------------
era5_block <- imap(LEVELS, function(lvl, iso) {
  tbl(con, "era5") |> filter(iso3 == iso, adm_level == lvl) |>
    select(iso3, pcode, valid_date, mean) |> collect()
}) |> list_rbind() |>
  mutate(mm = lubridate::days_in_month(valid_date) * mean,
         year = lubridate::year(valid_date), month = lubridate::month(valid_date)) |>
  filter(month %in% 5:6) |>
  group_by(iso3, pcode, year) |>
  filter(all(5:6 %in% month)) |>
  summarise(obs_mm = sum(mm), .groups = "drop")

# ---- Forecast: SEAS5 Jul+Aug from the July issuance, per unit-year ----------
seas5_block <- imap(LEVELS, function(lvl, iso) {
  tbl(con, "seas5") |> filter(iso3 == iso, adm_level == lvl) |>
    filter(leadtime <= 2) |>   # July issuance: Jul=lt0, Aug=lt1
    select(iso3, pcode, issued_date, valid_date, leadtime, mean) |> collect()
}) |> list_rbind() |>
  filter(lubridate::month(issued_date) == 7,
         lubridate::month(valid_date) %in% 7:8) |>
  mutate(mm = lubridate::days_in_month(valid_date) * mean,
         year = lubridate::year(issued_date)) |>
  group_by(iso3, pcode, year) |>
  filter(all(7:8 %in% lubridate::month(valid_date))) |>
  summarise(fcst_mm = sum(mm), .groups = "drop")

# ---- Z-score each block vs 1991-2024 baseline, then blend ------------------
zscore <- function(df, val) {
  df |> group_by(pcode) |>
    mutate(mu = mean(.data[[val]][year %in% BASE]),
           sig = sd(.data[[val]][year %in% BASE]),
           z = (.data[[val]] - mu) / sig) |>
    ungroup()
}
obs_z <- zscore(era5_block, "obs_mm") |> select(iso3, pcode, year, z_obs = z)
fc_z  <- zscore(seas5_block, "fcst_mm") |> select(pcode, year, z_fcst = z)

mixed <- inner_join(obs_z, fc_z, by = c("pcode", "year")) |>
  mutate(z_mixed = (z_obs + z_fcst) / 2)

# Rank 2026 blended z among all years (low z = dry). Weibull RP via msu_emp_rp (direction -1).
mixed_rp <- msu_emp_rp(mixed, var = "z_mixed", by = "pcode", direction = -1)

meta <- msu_admin_meta(adm_level = 1L) |> select(pcode, name)
meta0 <- msu_admin_meta(adm_level = 0L) |> select(pcode, name)
name_lu <- bind_rows(meta, meta0)

res_2026 <- mixed_rp |> filter(year == 2026) |>
  left_join(name_lu, by = "pcode") |>
  mutate(rp_class = msu_rp_class(rp_emp)) |>
  select(iso3, pcode, name, z_obs, z_fcst, z_mixed, pctile, rp_emp, rp_class)
readr::write_csv(res_2026, file.path(out_csv, "mixed_primera_2026.csv"))

cat("== Mixed primera 2026 (complete May-Aug), driest units by blended z ==\n")
res_2026 |> arrange(z_mixed) |>
  select(iso3, name, z_obs, z_fcst, z_mixed, pctile, rp_emp) |> head(15) |> as.data.frame() |> print(digits = 2)

# How much does the forecast half change the observed-only picture?
cat("\nObserved (May-Jun) vs forecast (Jul-Aug) z, correlation across 2026 units:",
    round(cor(res_2026$z_obs, res_2026$z_fcst), 2), "\n")
cat("Units where blended is drier than observed-only (forecast worsens):",
    sum(res_2026$z_mixed < res_2026$z_obs), "of", nrow(res_2026), "\n")

# ---- Map (adm1 GTM/HND + adm0 SLV) -----------------------------------------
lyr <- list(gtm = "gtm_adm1", hnd = "hnd_adm1", slv = "slv_adm0")
gdf <- imap(lyr, function(l, iso) {
  g <- cumulus::download_fieldmaps_sf(iso3 = iso, layer = l)[[l]]
  names(g)[names(g) == "geom"] <- "geometry"; st_geometry(g) <- "geometry"
  g <- janitor::clean_names(g)
  pcol <- names(g)[grepl("adm[01]_pcode", names(g))][1]
  g |> rename(pcode = all_of(pcol)) |> select(pcode, geometry)
}) |> bind_rows()

map_df <- left_join(gdf, res_2026, by = "pcode") |> mutate(iso3c = substr(pcode, 1, 2))
p_rp <- map_df |> filter(!is.na(rp_class)) |>
  ggplot() + geom_sf(aes(fill = rp_class), color = "grey80", linewidth = 0.1) +
  scale_fill_manual(values = MSU_RP_FILL, drop = FALSE, na.value = "grey90", name = "Dry return\nperiod (yr)") +
  labs(title = "Complete 2026 primera (observed + forecast) — dry return period",
       subtitle = "ERA5 May-Jun observed blended with SEAS5 Jul-Aug forecast (z-score blend). adm1 GTM/HND, adm0 SLV.\nDarker = rarer/drier once the full primera is accounted for.",
       caption = "Blended z ranked 1981-2026 (baseline 1991-2024). RP=(n+1)/rank, Weibull.") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
msu_save_png(p_rp, file.path(out_png, "01_mixed_primera_rp_map.png"), width = 10, height = 6)

# Observed vs forecast contribution scatter
p_sc <- res_2026 |>
  ggplot(aes(x = z_obs, y = z_fcst, color = iso3)) +
  geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_point(size = 2.5, alpha = 0.85) +
  ggrepel::geom_text_repel(aes(label = name), size = 2.6, max.overlaps = 14, show.legend = FALSE) +
  labs(title = "2026 primera: observed May-Jun vs forecast Jul-Aug anomaly",
       subtitle = "z-scores per unit. Both negative (bottom-left) = dry start AND dry forecast = strongest concern.",
       x = "Observed May-Jun z (ERA5)", y = "Forecast Jul-Aug z (SEAS5, Jul issuance)") +
  theme(legend.position = "top")
msu_save_png(p_sc, file.path(out_png, "02_obs_vs_forecast_scatter.png"), width = 9, height = 7)
