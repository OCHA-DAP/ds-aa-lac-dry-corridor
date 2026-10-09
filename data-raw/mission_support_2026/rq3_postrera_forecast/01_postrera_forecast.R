# RQ3 — Postrera (SON) forecast outlook + the primera-vs-postrera "second chance" view.
#
# Postrera = Sep-Oct-Nov, fully forecastable from the July SEAS5 issuance (lt 2,3,4). Pure
# forecast (Tier-2, resolution: adm1 GTM/HND, adm0 SLV per ADR-0001). Standardise SON forecast
# vs its own July-issuance hindcast (1991-2024), z-score + empirical RP (ranked within
# 1991-2024 + 2026, MSU_RP_YEARS).
#
# Then join to the rq2 COMPLETE primera (obs May-Jun + forecast Jul-Aug) for the double-hit view:
# a unit dry in the complete primera AND forecast-dry in postrera has no "second chance".

source(file.path("data-raw/mission_support_2026/_shared/funcs.R"))
library(purrr)

out_png <- "data-raw/mission_support_2026/rq3_postrera_forecast/png"
out_csv <- "data-raw/mission_support_2026/rq3_postrera_forecast"
BASE <- 1991:2024
con <- msu_con()
# adm1 for all three (match rq2 mixed adm1 so the primera-vs-postrera join is at adm1). SLV
# postrera forecast is coarse SEAS5 (sub-pixel) -> indicative within SLV.
LEVELS <- list(GTM = 1L, HND = 1L, SLV = 1L)

# ---- SEAS5 SON (postrera) from the July issuance ---------------------------
seas5_son <- imap(LEVELS, function(lvl, iso) {
  tbl(con, "seas5") |> filter(iso3 == iso, adm_level == lvl, leadtime <= 4) |>
    select(iso3, pcode, issued_date, valid_date, leadtime, mean) |> collect()
}) |> list_rbind() |>
  filter(lubridate::month(issued_date) == 7, lubridate::month(valid_date) %in% 9:11) |>
  mutate(mm = lubridate::days_in_month(valid_date) * mean, year = lubridate::year(issued_date)) |>
  group_by(iso3, pcode, year) |>
  filter(all(9:11 %in% lubridate::month(valid_date))) |>
  summarise(son_mm = sum(mm), .groups = "drop")

post_z <- seas5_son |> group_by(pcode) |>
  mutate(mu = mean(son_mm[year %in% BASE]), sig = sd(son_mm[year %in% BASE]),
         z_post = (son_mm - mu) / sig) |> ungroup()

post_rp <- msu_emp_rp(filter(post_z, year %in% MSU_RP_YEARS), var = "z_post", by = "pcode",
                      direction = -1)
meta <- bind_rows(msu_admin_meta(adm_level = 1L) |> select(pcode, name),
                  msu_admin_meta(adm_level = 0L) |> select(pcode, name))

post_2026 <- post_rp |> filter(year == 2026) |> left_join(meta, by = "pcode") |>
  select(iso3, pcode, name, z_post, pctile_post = pctile, rp_post = rp_emp)
readr::write_csv(post_2026, file.path(out_csv, "postrera_forecast_2026.csv"))

cat("== Postrera SON forecast 2026: driest units ==\n")
post_2026 |> arrange(z_post) |> select(iso3, name, z_post, rp_post) |> head(10) |> as.data.frame() |> print(digits = 2)

# ---- Join to complete primera (rq2) for the double-hit view ----------------
prim <- readr::read_csv("data-raw/mission_support_2026/rq2_primera_mixed/mixed_primera_2026.csv",
                        show_col_types = FALSE) |>
  select(pcode, name, iso3, z_primera = z_mixed, rp_primera = rp_emp)

joint <- inner_join(prim, post_2026 |> select(pcode, z_post, rp_post), by = "pcode")
readr::write_csv(joint, file.path(out_csv, "primera_vs_postrera_2026.csv"))
cat("\nprimera vs postrera z correlation (2026 units):", round(cor(joint$z_primera, joint$z_post), 2), "\n")
cat("Units dry in BOTH (z<0 both):", sum(joint$z_primera < 0 & joint$z_post < 0), "of", nrow(joint), "\n")
