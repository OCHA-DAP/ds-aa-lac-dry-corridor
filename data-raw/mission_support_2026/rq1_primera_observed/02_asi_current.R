# RQ1 script 02 — FAO ASI (Agricultural Stress Index) current, admin-1, crop area.
#
# The independent IMPACT lens (vegetation-based, over CROP AREA), complementing the rainfall
# lens. Because ASI is crop-focused by construction, it inherently addresses "is anyone
# actually farming here" — so it helps localize where the broad rainfall deficit is translating
# into agricultural stress. ASI value = % of crop area in severe stress (higher = worse).
# Dekadal, current through 2026-06-21.

source(file.path("data-raw/mission_support_2026/_shared/funcs.R"))
library(purrr)
library(sf)

out_png <- "data-raw/mission_support_2026/rq1_primera_observed/png"
out_csv <- "data-raw/mission_support_2026/rq1_primera_observed"

asi <- cumulus::fao_asi_adm1_tabular(iso3 = c("GTM", "HND", "SLV")) |>
  janitor::clean_names() |>
  filter(land_type == "Crop Area") |>
  # ASI province names arrive as Latin-1 bytes (e.g. "Pet\xe9n"); repair to UTF-8 so accents
  # transliterate correctly in the name join (else é/á are dropped -> petn, quich, ...).
  mutate(province = stringi::stri_conv(province, "latin1", "UTF-8"),
         iso3 = dplyr::recode(country, Guatemala = "GTM", Honduras = "HND", `El Salvador` = "SLV"))

# ASI is a CUMULATIVE season-to-date indicator (per Zack): the latest dekad already encodes
# accumulated stress, so we take the FINAL JUNE DEKAD (day 21), not a mean over dekads. Compare
# the same dekad across years for an apples-to-apples "as of late June" cumulative read.
TARGET_MONTH <- 6L; TARGET_DEKAD <- 3L   # dekad coded 1/2/3 within month; 3 = the 21st (final June dekad)
asi_primera <- asi |>
  filter(month(date) == TARGET_MONTH, dekad == TARGET_DEKAD) |>
  group_by(iso3, province, adm1_code, year) |>
  summarise(asi_mean = mean(data), .groups = "drop")   # mean() only collapses any dup rows; 1 per unit-year

# Rank 2026's late-June cumulative ASI within each unit's own history (higher ASI = worse)
asi_rank <- asi_primera |>
  group_by(iso3, province, adm1_code) |>
  mutate(n_years = n(),
         asi_pctile = rank(asi_mean, ties.method = "min") / (n_years + 1)) |>
  ungroup()

asi_2026 <- asi_rank |> filter(year == 2026) |>
  select(iso3, province, adm1_code, asi_mean_2026 = asi_mean, asi_pctile, n_years)

readr::write_csv(asi_2026, file.path(out_csv, "asi_primera_2026.csv"))

cat("== Most agriculturally stressed adm1 units, ASI as of late June 2026 (cumulative, %) ==\n")
asi_2026 |> arrange(desc(asi_mean_2026)) |>
  select(iso3, province, asi_mean_2026, asi_pctile) |> head(15) |> as.data.frame() |> print(digits = 3)
cat("\nLate-June ASI by country (2026 vs historical mean for the same dekad):\n")
asi_primera |> group_by(iso3) |>
  summarise(asi_2026 = mean(asi_mean[year == 2026]),
            asi_hist = mean(asi_mean[year < 2026]), .groups = "drop") |>
  as.data.frame() |> print(digits = 3)

# --- Map: 2026 primera-to-date ASI by adm1 ----------------------------------
iso_lower <- c("gtm", "hnd", "slv")
gdf_adm1 <- iso_lower |>
  map(\(i) cumulus::download_fieldmaps_sf(iso3 = i, layer = paste0(i, "_adm1"))[[paste0(i, "_adm1")]]) |>
  map(\(g) { names(g)[names(g) == "geom"] <- "geometry"; st_geometry(g) <- "geometry"; g }) |>
  map(\(g) janitor::clean_names(g)) |>
  map(\(g) {
    pcol <- names(g)[grepl("adm1_pcode", names(g))][1]
    ncol <- names(g)[grepl("adm1_(es|en|name)", names(g))][1]
    g |> rename(pcode = all_of(pcol), adm1_name = all_of(ncol)) |> select(pcode, adm1_name, geometry)
  }) |>
  bind_rows()

# join ASI to boundaries by iso3 + robust name key (accents/case/articles) — msu_name_key
# handles Quiché->quiche, El Paraíso->paraiso, etc. iso3 avoids cross-country name collisions.
pref2iso <- c(GT = "GTM", HN = "HND", SV = "SLV")
map_df <- gdf_adm1 |>
  mutate(iso3 = unname(pref2iso[substr(pcode, 1, 2)]), jn = msu_name_key(adm1_name)) |>
  left_join(asi_2026 |> mutate(jn = msu_name_key(province)) |>
              select(iso3, jn, asi_mean_2026, asi_pctile),
            by = c("iso3", "jn"))

# Categorise every admin unit: analysed, or a stated reason it isn't. ASI reports only units
# with crop area, so a unit absent from ASI = negligible cropland (not a data gap on our side).
map_df <- map_df |>
  mutate(status = if_else(is.na(asi_mean_2026), "No ASI (insufficient cropland)", "Analysed"))
n_unmatched <- sum(map_df$status != "Analysed")
cat("\nunits without ASI (shown hatched):", n_unmatched, "of", nrow(map_df), "\n")
if (n_unmatched > 0) cat("  ", paste(map_df$adm1_name[map_df$status != "Analysed"], collapse = ", "), "\n")

gghdx_ok <- requireNamespace("gghdx", quietly = TRUE)
if (gghdx_ok) gghdx::gghdx()

analysed <- map_df |> filter(status == "Analysed")
not_analysed <- map_df |> filter(status != "Analysed")
hatch <- if (nrow(not_analysed) > 0) msu_hatch(not_analysed) else NULL

p_map <- ggplot() +
  # all boundaries as base (so unanalysed units still show an outline)
  geom_sf(data = map_df, fill = "grey95", color = "grey80", linewidth = 0.1) +
  geom_sf(data = analysed, aes(fill = asi_mean_2026), color = "grey80", linewidth = 0.1) +
  { if (!is.null(hatch)) geom_sf(data = hatch, aes(color = "No ASI (insufficient cropland)"),
                                 linewidth = 0.2) } +
  facet_wrap(~iso3, nrow = 1) +
  scale_fill_gradient(low = "#fff5f0", high = "#67000d", name = "ASI 2026\n(% crop area\nstressed)") +
  scale_color_manual(values = c("No ASI (insufficient cropland)" = "grey55"), name = NULL) +
  labs(title = "Agricultural stress (FAO ASI) over crop area, as of late June 2026",
       subtitle = "Cumulative ASI at the final June dekad (2026-06-21), admin-1. Darker = more crop area in severe stress.\nHatched = unit not analysed (reason in legend).",
       caption = "FAO ASI (crop area), season-to-date cumulative. Independent vegetation-based impact proxy.") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
msu_save_png(p_map, file.path(out_png, "04_asi_primera_2026_map.png"), width = 11, height = 4.5)
