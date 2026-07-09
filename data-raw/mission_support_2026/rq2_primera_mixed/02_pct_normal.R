# RQ2b — % of normal (value / 1991-2024 mean) per adm1, for the summary-chapter table.
# Mirrors the seasonal blocks used for the z-scores (rq2 mixed, rq3 postrera):
#   observed  = ERA5 May+Jun ; combined = ERA5 May+Jun + SEAS5 Jul+Aug (July issuance) ;
#   postrera  = SEAS5 Sep+Oct+Nov (July issuance). Writes pct_normal_2026.csv (gitignored, local).

source(file.path("data-raw/mission_support_2026/_shared/funcs.R"))
library(purrr)
BASE <- 1991:2024; con <- msu_con()
LEVELS <- list(GTM = 1L, HND = 1L, SLV = 1L)
m  <- lubridate::month; y <- lubridate::year; dim_ <- lubridate::days_in_month

era5 <- imap(LEVELS, \(lvl, iso) tbl(con, "era5") |> filter(iso3 == iso, adm_level == lvl) |>
    select(iso3, pcode, valid_date, mean) |> collect()) |> list_rbind() |>
  mutate(mm = dim_(valid_date) * mean, year = y(valid_date), month = m(valid_date)) |>
  filter(month %in% 5:6) |> group_by(iso3, pcode, year) |> filter(all(5:6 %in% month)) |>
  summarise(obs_mm = sum(mm), .groups = "drop")

seas5 <- function(lts, mons) imap(LEVELS, \(lvl, iso) tbl(con, "seas5") |>
    filter(iso3 == iso, adm_level == lvl, leadtime <= lts) |>
    select(iso3, pcode, issued_date, valid_date, leadtime, mean) |> collect()) |> list_rbind() |>
  filter(m(issued_date) == 7, m(valid_date) %in% mons) |>
  mutate(mm = dim_(valid_date) * mean, year = y(issued_date)) |>
  group_by(iso3, pcode, year) |> filter(all(mons %in% m(valid_date)))
ja  <- seas5(2, 7:8)  |> summarise(fcst_mm = sum(mm), .groups = "drop")
son <- seas5(4, 9:11) |> summarise(son_mm  = sum(mm), .groups = "drop")
comb <- inner_join(era5, ja |> select(pcode, year, fcst_mm), by = c("pcode","year")) |>
  mutate(comb_mm = obs_mm + fcst_mm)

pn <- function(df, val, nm) df |> group_by(pcode) |>
  mutate(mu = mean(.data[[val]][year %in% BASE]), p = .data[[val]] / mu * 100) |> ungroup() |>
  filter(year == 2026) |> transmute(pcode, !!nm := round(p, 0))

out <- pn(era5, "obs_mm", "obs_pnorm") |>
  full_join(pn(comb, "comb_mm", "comb_pnorm"), by = "pcode") |>
  full_join(pn(son,  "son_mm",  "post_pnorm"), by = "pcode")
readr::write_csv(out, "data-raw/mission_support_2026/rq2_primera_mixed/pct_normal_2026.csv")
cat("wrote pct_normal_2026.csv,", nrow(out), "units\n"); print(as.data.frame(head(out)))
cat("ranges: obs", paste(range(out$obs_pnorm,na.rm=TRUE),collapse="-"),
    "| comb", paste(range(out$comb_pnorm,na.rm=TRUE),collapse="-"),
    "| post", paste(range(out$post_pnorm,na.rm=TRUE),collapse="-"), "%\n")
