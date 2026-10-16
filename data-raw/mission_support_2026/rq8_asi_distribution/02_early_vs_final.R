# RQ8.2 — Does END-JUNE ASI predict the FINAL-season outcome?
#
# Validates acting on the late-June signal now. For each unit-year: early = end-June ASI
# (month 6 dekad 3); final = the season's eventual PEAK stress AFTER June (max ASI over Jul-Oct,
# months 7-10). If early predicts final, flagging Olancho etc. today is justified.

source(file.path("data-raw/mission_support_2026/_shared/funcs.R"))
library(ggrepel)
out <- "data-raw/mission_support_2026/rq8_asi_distribution"

asi <- cumulus::fao_asi_adm1_tabular(iso3 = c("GTM", "HND", "SLV")) |>
  janitor::clean_names() |> filter(land_type == "Crop Area") |>
  mutate(province = stringi::stri_conv(province, "latin1", "UTF-8"),
         iso3 = dplyr::recode(country, Guatemala = "GTM", Honduras = "HND", `El Salvador` = "SLV"),
         mon = lubridate::month(date))

early <- asi |> filter(mon == 6, dekad == 3) |> transmute(iso3, province, year, early = data)
final <- asi |> filter(mon %in% 7:10) |> group_by(iso3, province, year) |>
  summarise(final = max(data, na.rm = TRUE), .groups = "drop")

d <- inner_join(early, final, by = c("iso3", "province", "year")) |> filter(is.finite(final))
hist <- d |> filter(year < 2026)

# overall predictive skill
cat("== Early (end-June) -> Final (Jul-Oct peak) ASI, historical (pre-2026) ==\n")
cat("Pearson r:", round(cor(hist$early, hist$final), 2),
    "| Spearman:", round(cor(hist$early, hist$final, method = "spearman"), 2), "\n")

# per-unit correlation
percorr <- hist |> group_by(iso3, province) |>
  summarise(r = cor(early, final), n = n(), .groups = "drop")
cat("per-unit Pearson r: median", round(median(percorr$r, na.rm = TRUE), 2),
    "| range", paste(round(range(percorr$r, na.rm = TRUE), 2), collapse = " to "), "\n")

# does a HIGH end-June flag a bad final? define "bad final" = top-tercile final per unit;
# "high early" = top-tercile early per unit. P(bad final | high early) vs base rate 1/3.
flag <- hist |> group_by(iso3, province) |>
  mutate(hi_early = early >= quantile(early, 2/3), bad_final = final >= quantile(final, 2/3)) |>
  ungroup()
cat("\nP(bad final):", round(mean(flag$bad_final), 2),
    "| P(bad final | high end-June):", round(mean(flag$bad_final[flag$hi_early]), 2),
    "| lift:", round(mean(flag$bad_final[flag$hi_early]) / mean(flag$bad_final), 2), "\n")

# 2026 early values -> implied risk
cur <- early |> filter(year == 2026) |> arrange(desc(early))
cat("\n2026 end-June ASI (the signal we're acting on), top units:\n")
print(as.data.frame(head(cur, 8)), digits = 3)

readr::write_csv(d, file.path(out, "early_vs_final_asi.csv"))

# scatter
gghdx_ok <- requireNamespace("gghdx", quietly = TRUE); if (gghdx_ok) gghdx::gghdx()
lab <- hist |> filter(early > 30 | final > 60)
p <- hist |>
  ggplot(aes(early, final)) +
  geom_point(alpha = 0.35, size = 1.2, color = "grey40") +
  geom_smooth(method = "lm", se = TRUE, color = "#d7301f", linewidth = 0.8) +
  labs(title = "Does end-June ASI predict the rest of the season?",
       subtitle = sprintf("Each point = one admin-1 × year (1984–2025). r = %.2f. Higher end-June stress → higher peak stress.",
                          cor(hist$early, hist$final)),
       x = "End-June ASI (early signal, % crop area stressed)",
       y = "Peak ASI Jul–Oct (season outcome)") +
  theme(legend.position = "none")
msu_save_png(p, file.path(out, "png/02_early_vs_final_scatter.png"), width = 8.5, height = 6.5)
