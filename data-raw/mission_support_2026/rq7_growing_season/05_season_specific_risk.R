# RQ7.6 — Season-specific agricultural drought risk: primera / postrera / both.
#
# PSP marks May-Nov as ONE rain-critical period here (ASAP can't split primera vs postrera), so
# PSP is the GATE (is this zone rain-sensitive in our windows + real cropland). The primera-vs-
# postrera distinction comes from the season-specific HAZARD:
#   - PRIMERA risk = REALIZED: crops already stressed now (FAO ASI, the primera-to-date impact).
#   - POSTRERA risk = FORECAST: SEAS5 SON forecast driest (low confidence, see skill).
# Classify PSP-gated, crop-exposed zones into Primera / Postrera / Both / Lower; off-PSP zones
# (highlands, Caribbean) -> "Off-calendar" (seasonal lens doesn't apply -> vulnerability question).

source(file.path("data-raw/mission_support_2026/_shared/funcs.R"))
rq7 <- "data-raw/mission_support_2026/rq7_growing_season"
rq5 <- "data-raw/mission_support_2026/rq5_crop_vulnerability"
rq3 <- "data-raw/mission_support_2026/rq3_postrera_forecast"

priority <- readr::read_csv(file.path(rq5, "priority_synthesis_adm1.csv"), show_col_types = FALSE) |>
  select(pcode, gaul_code, iso3, name, asi_2026, obs_pctile, enso_anom)
psp <- readr::read_csv(file.path(rq7, "psp_overlap_by_adm1.csv"), show_col_types = FALSE) |>
  transmute(gaul_code, psp = pmax(primera_psp, postrera_psp, na.rm = TRUE))
gs <- readr::read_csv(file.path(rq7, "growing_season_by_adm1.csv"), show_col_types = FALSE) |>
  select(gaul_code, crop_frac)
post <- readr::read_csv(file.path(rq3, "primera_vs_postrera_2026.csv"), show_col_types = FALSE) |>
  select(pcode, z_post)

ASI_ACT <- 15   # % crop area in severe stress = material realized primera impact

ag <- priority |>
  left_join(psp, by = "gaul_code") |> left_join(gs, by = "gaul_code") |>
  left_join(post, by = "pcode") |>
  filter(!is.na(psp), !is.na(z_post)) |>
  mutate(
    psp_relevant = psp >= 0.5,
    crop_exposed = crop_frac >= 0.10,
    # PRIMERA (realized): material current crop stress
    primera_risk = !is.na(asi_2026) & asi_2026 >= ASI_ACT,
    # POSTRERA (forecast): driest-tercile SON forecast among the gated zones
    postrera_risk = z_post <= quantile(z_post[psp_relevant & crop_exposed], 1/3, na.rm = TRUE),
    season_class = case_when(
      !(psp_relevant & crop_exposed) ~ "Off-calendar",
      primera_risk & postrera_risk   ~ "Both",
      primera_risk                   ~ "Primera",
      postrera_risk                  ~ "Postrera",
      TRUE                           ~ "Lower"
    ),
    season_class = factor(season_class, levels = c("Both", "Primera", "Postrera", "Lower", "Off-calendar"))
  )
readr::write_csv(ag, file.path(rq7, "season_specific_risk_adm1.csv"))

cat("== Season-specific class counts ==\n"); print(table(ag$season_class, useNA = "ifany"))
for (cl in c("Both", "Primera", "Postrera", "Off-calendar")) {
  cat("\n== ", cl, " ==\n")
  ag |> filter(season_class == cl) |> arrange(desc(asi_2026)) |>
    transmute(iso3, name, asi = round(asi_2026), crop_frac = round(crop_frac, 2),
              psp = round(psp, 2), z_post = round(z_post, 2), enso = round(enso_anom, 2)) |>
    as.data.frame() |> print()
}

# ---- map (artefact review; polished stacked version goes in the QMD) ----
suppressMessages({ library(sf); library(ggplot2) })
gaul <- st_read("data-raw/mission_support_2026/_shared/gaul_adm1_gtm_hnd_slv.geojson", quiet = TRUE) |>
  janitor::clean_names()
pal <- c(Both="#67000d", Primera="#d7301f", Postrera="#fdae61", Lower="#fee8c8", `Off-calendar`="grey75")
m <- left_join(gaul, ag |> select(gaul_code, season_class), by=c("adm1_code"="gaul_code")) |>
  mutate(iso3 = c(Guatemala="GTM",Honduras="HND",`El Salvador`="SLV")[adm0_name]) |> filter(!is.na(season_class))
p <- ggplot(m) + geom_sf(aes(fill=season_class), color="grey60", linewidth=0.1) +
  facet_wrap(~iso3, nrow=1) +
  scale_fill_manual(values=pal, name="Season risk") +
  labs(title="Season-specific agricultural drought risk (2026)",
       subtitle="Primera=realized (ASI now); Postrera=forecast (low skill); Both; Off-calendar=highlands/Caribbean (vuln. lens)") +
  theme(axis.text=element_blank(), axis.ticks=element_blank(), panel.grid=element_blank())
ggsave("data-raw/mission_support_2026/rq7_growing_season/png/05_season_specific_risk_map.png", p, width=11, height=4.5, dpi=150, bg="white")
message("wrote season-specific risk map")
