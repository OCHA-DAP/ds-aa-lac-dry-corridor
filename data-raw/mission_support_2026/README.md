# Mission-support 2026 — data pipeline

Scripts that produce the derived inputs for the book chapter
`analysis/2026_cadc_drought_v3/13_rolac_summary.qmd`. The chapter reads the outputs from blob
(`projects` container, prefix `ds-aa-lac-dry-corridor/book/static/`), so it renders without this
pipeline present. These scripts document and reproduce that data. Run from the repo root.

## Credentials

- **DB pulls** use `cumulus::pg_con()` — need `DSCI_AZ_DB_PROD_UID/HOST/PW`.
- **GEE pulls** use `rgee` — need an authenticated Earth Engine session.
- **Raster steps** read ASAP / PSP rasters from `AA_DATA_DIR` and the sibling `../ds-eo-rasters` repo.
- **Blob upload** uses `cumulus::blob_write` — needs `DSCI_AZ_BLOB_DEV_SAS_WRITE`.

None of these are required to *view* the book (the committed `_freeze/` output covers that); they are
required to regenerate the data or re-render from scratch.

## Run order (three tiers)

**Tier 1 — pulls (need DB / GEE / rasters).** Independent; can run in any order.

| Script | Produces | Source |
|---|---|---|
| `_shared/pull_gaul_geometries.R` | `gaul_adm1_gtm_hnd_slv.geojson` | GEE (GAUL) |
| `_shared/pull_cropland_gee.R` | cropland fraction per adm1 | GEE |
| `rq1_primera_observed/01_primera_observed.R` | `primera_observed_2026.csv` | DB: era5, polygon |
| `rq1_primera_observed/02_asi_current.R` | `asi_primera_2026.csv` | FAO ASI API |
| `rq2_primera_mixed/01_mixed_primera.R` | `mixed_primera_2026.csv` | DB: era5, seas5, polygon |
| `rq2_primera_mixed/02_pct_normal.R` | `pct_normal_2026.csv` | DB: era5, seas5 |
| `rq4_enso_context/01_enso_analog_composite.R` | ENSO composite | DB: era5 + ONI |
| `rq7_growing_season/01_growing_season_mask.R` | growing-season overlap | ASAP crop-mask raster |
| `rq7_growing_season/02_psp_overlap.R` | PSP overlap | `../ds-eo-rasters` PSP raster |

**Tier 2 — transforms (read Tier-1 CSVs only).**

| Script | Produces | Reads |
|---|---|---|
| `rq3_postrera_forecast/01_postrera_forecast.R` | `primera_vs_postrera_2026.csv` | rq2/01 (also pulls DB: seas5, polygon) — **must run after rq2/01** |
| `rq5_crop_vulnerability/01_priority_synthesis.R` | `priority_synthesis_adm1.csv` | rq1, rq4, cropland |
| `rq7_growing_season/05_season_specific_risk.R` | `season_specific_risk_adm1.csv` | rq5, rq7/01-02, rq3 |
| `rq8_asi_distribution/02_early_vs_final.R` | `early_vs_final_asi.csv` | FAO ASI API |

**Tier 3 — publish.** `99_upload_derived_to_blob.R` reads the nine derived files from the local
staging dir `analysis/2026_cadc_drought_v3/data/mission_support/` (gitignored) and `blob_write`s them
to `ds-aa-lac-dry-corridor/book/static/`. The chapter then reads them via `cumulus::blob_read`.

## Notes / gotchas

- Baseline **1991–2024** and the **SEAS5 July issuance** are hard-coded across the rainfall scripts and
  must stay aligned between the observed, combined, postrera, and percent-of-normal steps.
- Products are joined pcode↔GAUL through the `priority_synthesis_adm1.csv` crosswalk (`pcode`,
  `gaul_code`); regenerate rq5 whenever upstream units change, or joins silently drop rows.
- Tier-1/2 scripts write into their own `rqN` folders; the nine files the chapter uses are collected
  into the staging dir before upload. Collection is manual today — refresh it before running Tier 3.
- These files were promoted from the gitignored `artefacts/mission_support_2026/` tree; internal paths
  were rewired to `data-raw/mission_support_2026/`.
