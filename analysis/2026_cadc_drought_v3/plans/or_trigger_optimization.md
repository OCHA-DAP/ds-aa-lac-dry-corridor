# Plan: Optimize OR-Based Trigger Thresholds for Postrera

## Problem Statement

Current Postrera F1 scores are low when using the same RP threshold (RP=4) for both leadtimes. The operational trigger uses OR logic: **activate if LT0 OR LT1 predicts drought**.

**Key insight**: We can use different RP thresholds per leadtime:
- **LT1 (longer lead, less accurate)**: Higher RP = more conservative, fewer false alarms
- **LT0 (shorter lead, more accurate)**: Lower RP = catch more droughts

## Objective

Find the optimal combination of (RP_LT0, RP_LT1) that maximizes F1 when using OR logic for Postrera.

---

## Implementation Approach

### Analysis Logic

For each combination of thresholds:
1. Classify each year at LT0: `drought_LT0 = fcst_mm_LT0 <= threshold_LT0`
2. Classify each year at LT1: `drought_LT1 = fcst_mm_LT1 <= threshold_LT1`
3. Apply OR logic: `predicted_drought = drought_LT0 | drought_LT1`
4. Compare to observed drought (fixed at RP=4), calculate F1

### Search Space
- **RP_LT0**: 2.5 to 6 by 0.25 (LT0 is more accurate, can be more aggressive)
- **RP_LT1**: 3 to 8 by 0.25 (LT1 is less accurate, allow more conservative)

This gives a 2D grid of ~15 × 21 = 315 combinations per country.

### Visualization
- **2D heatmap**: RP_LT0 on x-axis, RP_LT1 on y-axis, F1 as color
- **Faceted by country**: Guatemala, Honduras, El Salvador
- **Mark optimal point** on each heatmap
- **Reference lines** at RP=4 (current threshold) for both axes

---

## Code to Add to Chapter 07

### Location
Add after the "F1 Score vs RP Threshold" section, before "## Recommendations"

### New Section Title: "### OR-Based Trigger Optimization (Postrera)"

```r
# Search over 2D threshold space for OR-based trigger
rp_lt0_grid <- seq(2.5, 6, by = 0.25)
rp_lt1_grid <- seq(3, 8, by = 0.25)
threshold_grid <- expand.grid(rp_lt0 = rp_lt0_grid, rp_lt1 = rp_lt1_grid)

calc_or_trigger_f1 <- function(df, rp_lt0, rp_lt1, baseline_start = 1981, baseline_end = 2024) {

  df_baseline <- df |>
    filter(year >= baseline_start, year <= baseline_end, window == "postrera")

  # Get obs threshold (fixed at RP=4 - definition of drought doesn't change)
  obs_thresh_df <- df_baseline |>
    group_by(country_aoi) |>
    summarise(obs_thresh = calc_rp_threshold(obs_mm, 4, -1), .groups = "drop")

  # Get forecast thresholds per leadtime (these we optimize)
  fcst_thresh_lt0 <- df_baseline |>
    filter(leadtime == 0) |>
    group_by(country_aoi) |>
    summarise(fcst_thresh_lt0 = calc_rp_threshold(fcst_mm, rp_lt0, -1), .groups = "drop")

  fcst_thresh_lt1 <- df_baseline |>
    filter(leadtime == 1) |>
    group_by(country_aoi) |>
    summarise(fcst_thresh_lt1 = calc_rp_threshold(fcst_mm, rp_lt1, -1), .groups = "drop")

  # Pivot to wide format (one row per country-year)
  df_wide <- df_baseline |>
    filter(leadtime %in% c(0, 1)) |>
    select(country_aoi, year, leadtime, fcst_mm, obs_mm) |>
    pivot_wider(names_from = leadtime, values_from = fcst_mm, names_prefix = "fcst_lt") |>
    left_join(obs_thresh_df, by = "country_aoi") |>
    left_join(fcst_thresh_lt0, by = "country_aoi") |>
    left_join(fcst_thresh_lt1, by = "country_aoi") |>
    mutate(
      obs_drought = obs_mm <= obs_thresh,
      fcst_drought_lt0 = fcst_lt0 <= fcst_thresh_lt0,
      fcst_drought_lt1 = fcst_lt1 <= fcst_thresh_lt1,
      # OR logic: trigger if either LT predicts drought
      fcst_drought_or = fcst_drought_lt0 | fcst_drought_lt1
    )

  # Calculate F1 per country using yardstick
  drought_levels <- c("drought", "no_drought")

  df_wide |>
    mutate(
      truth = fct(if_else(obs_drought, "drought", "no_drought"), levels = drought_levels),
      estimate = fct(if_else(fcst_drought_or, "drought", "no_drought"), levels = drought_levels)
    ) |>
    group_by(country_aoi) |>
    summarise(
      f1 = f_meas_vec(truth, estimate, event_level = "first"),
      precision = precision_vec(truth, estimate, event_level = "first"),
      recall = recall_vec(truth, estimate, event_level = "first"),
      .groups = "drop"
    ) |>
    mutate(rp_lt0 = rp_lt0, rp_lt1 = rp_lt1)
}

# Run grid search
df_or_grid <- map2_dfr(
  threshold_grid$rp_lt0,
  threshold_grid$rp_lt1,
  ~calc_or_trigger_f1(df_joined, .x, .y)
)
```

### Visualization: 2D Heatmap

```r
# Find optimal per country
df_optimal <- df_or_grid |>
  group_by(country_aoi) |>
  slice_max(f1, n = 1) |>
  slice(1)  # Handle ties

df_or_grid |>
  mutate(country_aoi = factor(country_aoi, levels = c("Guatemala", "Honduras", "El Salvador"))) |>
  ggplot(aes(x = rp_lt0, y = rp_lt1, fill = f1)) +
  geom_tile() +
  geom_point(data = df_optimal, color = "white", size = 3, shape = 4, stroke = 2) +
  geom_vline(xintercept = 4, linetype = "dashed", color = "white", alpha = 0.7) +
  geom_hline(yintercept = 4, linetype = "dashed", color = "white", alpha = 0.7) +
  facet_wrap(~country_aoi) +
  scale_fill_viridis_c(option = "plasma", limits = c(0, 0.7)) +
  labs(
    title = "Postrera: F1 for OR-Based Trigger (LT0 OR LT1)",
    subtitle = "X = optimal threshold combination. Dashed lines = current RP=4.",
    x = "RP Threshold for LT0 (Sep-issued)",
    y = "RP Threshold for LT1 (Aug-issued)",
    fill = "F1 Score",
    caption = "Baseline: 1981-2024. Observation drought threshold fixed at RP=4."
  ) +
  theme(
    strip.text = element_text(size = 11, face = "bold")
  )
```

### Summary Table

```r
# Show optimal thresholds and improvement over RP=4/RP=4
df_baseline_f1 <- df_or_grid |>
  filter(rp_lt0 == 4, rp_lt1 == 4) |>
  select(country_aoi, f1_baseline = f1)

df_optimal |>
  left_join(df_baseline_f1, by = "country_aoi") |>
  mutate(
    f1_improvement = f1 - f1_baseline,
    pct_improvement = (f1 - f1_baseline) / f1_baseline * 100
  ) |>
  select(country_aoi, rp_lt0, rp_lt1, f1, f1_baseline, f1_improvement, pct_improvement) |>
  gt() |>
  fmt_number(columns = c(f1, f1_baseline, f1_improvement), decimals = 3) |>
  fmt_number(columns = pct_improvement, decimals = 1, pattern = "{x}%") |>
  cols_label(
    country_aoi = "Country",
    rp_lt0 = "Optimal RP (LT0)",
    rp_lt1 = "Optimal RP (LT1)",
    f1 = "Optimal F1",
    f1_baseline = "Baseline F1 (RP=4)",
    f1_improvement = "Δ F1",
    pct_improvement = "% Improvement"
  )
```

---

### Average F1 Across Countries (for single operational threshold)

```r
# Average F1 across all countries for each threshold combination
df_or_avg <- df_or_grid |>
  group_by(rp_lt0, rp_lt1) |>
  summarise(
    f1_mean = mean(f1, na.rm = TRUE),
    f1_min = min(f1, na.rm = TRUE),
    f1_max = max(f1, na.rm = TRUE),
    .groups = "drop"
  )

# Find overall optimal
df_optimal_overall <- df_or_avg |>
  slice_max(f1_mean, n = 1) |>
  slice(1)

# Plot average F1 heatmap
ggplot(df_or_avg, aes(x = rp_lt0, y = rp_lt1, fill = f1_mean)) +
  geom_tile() +
  geom_point(data = df_optimal_overall, color = "white", size = 4, shape = 4, stroke = 2) +
  geom_vline(xintercept = 4, linetype = "dashed", color = "white", alpha = 0.7) +
  geom_hline(yintercept = 4, linetype = "dashed", color = "white", alpha = 0.7) +
  scale_fill_viridis_c(option = "plasma", limits = c(0, 0.6)) +
  labs(
    title = "Postrera: Average F1 Across Countries (OR Trigger)",
    subtitle = paste0("Optimal: RP_LT0 = ", df_optimal_overall$rp_lt0,
                      ", RP_LT1 = ", df_optimal_overall$rp_lt1,
                      " (mean F1 = ", round(df_optimal_overall$f1_mean, 3), ")"),
    x = "RP Threshold for LT0 (Sep-issued)",
    y = "RP Threshold for LT1 (Aug-issued)",
    fill = "Mean F1",
    caption = "Averaged over Guatemala, Honduras, El Salvador. X = optimal."
  )
```

---

## Expected Insights

- **Asymmetric optimal**: LT1 likely optimal at higher RP (more conservative) than LT0
- **Country differences**: Honduras may tolerate lower thresholds (better skill)
- **Overall optimal**: Single best threshold combination averaged across all countries
- **Improvement quantification**: How much does optimization improve over RP=4?

---

## Files to Modify

| File | Action |
|------|--------|
| `07_multi_aoi_comparison.qmd` | Add OR-trigger optimization section |

---

## Verification

1. Render chapter: `quarto render 07_multi_aoi_comparison.qmd`
2. Check heatmap shows smooth gradient (not noisy)
3. Verify optimal points are marked correctly
4. Compare optimal F1 to current RP=4 F1
5. Check that results make intuitive sense (LT1 threshold >= LT0 threshold)
