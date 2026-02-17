box::use(
  dplyr[...],
  tidyr[expand_grid],
  purrr[map_dfr, pmap_dbl],
  stats[approx, sd, na.omit, setNames],
  utils[head]
)

# ── Internal helper ──────────────────────────────────────────────────────────

#' Empirical RP to rainfall mm threshold via linear interpolation
#' @param x numeric vector of values (e.g. forecast mm)
#' @param rp_target target return period
#' @param direction -1 for drought (low = extreme), +1 for flood (high = extreme)
#' @return threshold value at the target RP
calc_rp_threshold <- function(x, rp_target = 4, direction = -1) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  ranks <- rank(x * -direction, ties.method = "average")
  rp <- (n + 1) / ranks
  approx(rp, x, xout = rp_target, rule = 2)$y
}

# ── Exported functions ───────────────────────────────────────────────────────

#' Generate all candidate threshold configurations
#'
#' Pure math — no data needed. Computes empirical RP breakpoints for a
#' given number of baseline years, then enumerates the full cross-join of
#' primera x postrera combinations. Optional LTs get NA as an option.
#' No filtering is applied — use `grid_constrain_*` functions to narrow.
#'
#' @param n_years integer, number of baseline years
#' @param primera_lts integer, number of primera leadtimes (2 or 3)
#' @param postrera_lts integer, number of postrera leadtimes (2, 3, or 4)
#' @param min_rp numeric, minimum RP to include in grid
#' @param max_rp numeric, maximum RP to include in grid
#' @return tibble with columns p0, p1, [p2], s0, s1, s2, [s3]
#' @export
generate_combos <- function(n_years,
                            primera_lts = 3,
                            postrera_lts = 4,
                            min_rp = 2.8,
                            max_rp = 15) {
  # Empirical RP breakpoints
  empirical_rps <- (n_years + 1) / seq_len(n_years)
  rp_opts <- empirical_rps[empirical_rps >= min_rp & empirical_rps <= max_rp]

  # -- Primera combos (p0 always required) --
  if (primera_lts >= 3) {
    p_grid <- expand_grid(
      p0 = rp_opts,
      p1 = c(NA_real_, rp_opts),
      p2 = c(NA_real_, rp_opts)
    )
  } else {
    p_grid <- expand_grid(
      p0 = rp_opts,
      p1 = c(NA_real_, rp_opts)
    )
  }

  # -- Postrera combos (s0 always required) --
  if (postrera_lts >= 4) {
    s_grid <- expand_grid(
      s0 = rp_opts,
      s1 = c(NA_real_, rp_opts),
      s2 = c(NA_real_, rp_opts),
      s3 = c(NA_real_, rp_opts)
    )
  } else if (postrera_lts >= 3) {
    s_grid <- expand_grid(
      s0 = rp_opts,
      s1 = c(NA_real_, rp_opts),
      s2 = c(NA_real_, rp_opts)
    )
  } else {
    s_grid <- expand_grid(
      s0 = rp_opts,
      s1 = c(NA_real_, rp_opts)
    )
  }

  # Cross-join primera x postrera
  tidyr::crossing(p_grid, s_grid)
}


#' Grid constraint: within-season monotonicity
#'
#' Pre-evaluation grid constraint (no data needed). Enforces that later
#' leadtimes (earlier forecasts) use thresholds at least as stringent
#' (higher RP) as shorter leadtimes: p0 <= p1 <= p2, s0 <= s1 <= s2 <= s3.
#' Also enforces trim-inward: can't skip an inner LT and include an outer
#' (e.g. p1=NA requires p2=NA).
#'
#' @param df tibble from generate_combos()
#' @return filtered tibble
#' @export
grid_constrain_season_monotonic <- function(df) {
  # Primera
  if ("p1" %in% names(df)) {
    df <- df |> filter(is.na(p1) | p1 >= p0)
  }
  if ("p2" %in% names(df)) {
    df <- df |>
      filter(is.na(p2) | (!is.na(p1) & p2 >= p1)) |>
      filter(!(is.na(p1) & !is.na(p2)))
  }

  # Postrera
  if ("s1" %in% names(df)) {
    df <- df |> filter(is.na(s1) | s1 >= s0)
  }
  if ("s2" %in% names(df)) {
    df <- df |>
      filter(is.na(s2) | (!is.na(s1) & s2 >= s1)) |>
      filter(!(is.na(s1) & !is.na(s2)))
  }
  if ("s3" %in% names(df)) {
    df <- df |>
      filter(is.na(s3) | (!is.na(s2) & s3 >= s2)) |>
      filter(!(is.na(s2) & !is.na(s3)))
  }

  df
}


#' Grid constraint: inter-season monotonicity
#'
#' Pre-evaluation grid constraint (no data needed): min(s thresholds) >= min(p thresholds).
#' Postrera has lower skill, so should be at least as strict as primera.
#'
#' @param df tibble from generate_combos()
#' @return filtered tibble
#' @export
grid_constrain_inter_season_monotonic <- function(df) {
  p_cols <- intersect(c("p0", "p1", "p2"), names(df))
  s_cols <- intersect(c("s0", "s1", "s2", "s3"), names(df))

  df |>
    mutate(
      .min_p = pmin(!!!syms(p_cols), na.rm = TRUE),
      .min_s = pmin(!!!syms(s_cols), na.rm = TRUE)
    ) |>
    filter(.min_s >= .min_p) |>
    select(-starts_with(".min_"))
}


#' Grid constraint: individual LT RP range
#'
#' Pre-evaluation grid constraint (no data needed). Removes rows where any
#' non-NA threshold falls outside [min_rp, max_rp]. Useful for narrowing the
#' grid before the expensive evaluate step.
#'
#' @param df tibble with threshold columns (p0, p1, ..., s0, s1, ...)
#' @param min_rp numeric, minimum RP allowed for any LT
#' @param max_rp numeric, maximum RP allowed for any LT
#' @return filtered tibble
#' @export
grid_constrain_lt_rp_range <- function(df, min_rp = 2.8, max_rp = 15) {
  lt_cols <- intersect(c("p0", "p1", "p2", "s0", "s1", "s2", "s3"), names(df))

  for (col in lt_cols) {
    df <- df |>
      filter(is.na(.data[[col]]) | (.data[[col]] >= min_rp & .data[[col]] <= max_rp))
  }
  df
}


#' Pre-compute trigger years for every (LT, RP) combination
#'
#' @param df_joined data.frame with columns: year, window, leadtime, fcst_mm, obs_mm
#' @param min_rp numeric, minimum RP for grid
#' @param max_rp numeric, maximum RP for grid
#' @param primera_lts integer, number of primera leadtimes (default 3: LT0, LT1, LT2)
#' @param postrera_lts integer, number of postrera leadtimes (default 3: LT0, LT1, LT2)
#' @return named list with: trigger_lookup, primera_ranked, postrera_ranked, n_years, rp_options
#' @export
build_trigger_lookup <- function(df_joined,
                                 min_rp = 2.8,
                                 max_rp = 15,
                                 primera_lts = 3,
                                 postrera_lts = 3) {
  # Define LT mapping
  lt_list <- list()
  for (i in seq_len(primera_lts)) {
    lt <- i - 1L
    lt_list <- c(lt_list, list(list(
      window = "primera", leadtime = lt, id = paste0("p", lt)
    )))
  }
  for (i in seq_len(postrera_lts)) {
    lt <- i - 1L
    lt_list <- c(lt_list, list(list(
      window = "postrera", leadtime = lt, id = paste0("s", lt)
    )))
  }

  # Observed years ranked driest-first per season
  obs_by_year <- df_joined |> distinct(year, window, obs_mm)
  primera_ranked <- obs_by_year |>
    filter(window == "primera") |> arrange(obs_mm) |> pull(year)
  postrera_ranked <- obs_by_year |>
    filter(window == "postrera") |> arrange(obs_mm) |> pull(year)

  n_years <- length(primera_ranked)

  # Build trigger lookup
  trigger_lookup <- list()
  rp_options <- list()

  for (lt_info in lt_list) {
    df_lt <- df_joined |>
      filter(window == lt_info$window, leadtime == lt_info$leadtime)
    n <- nrow(df_lt)
    if (n == 0) next

    empirical_rps <- (n + 1) / seq_len(n)
    in_range <- empirical_rps >= min_rp & empirical_rps <= max_rp
    rp_opts <- empirical_rps[in_range]
    rp_options[[lt_info$id]] <- rp_opts

    for (rp in rp_opts) {
      fcst_thresh <- calc_rp_threshold(df_lt$fcst_mm, rp, -1)
      trigger_years <- df_lt$year[df_lt$fcst_mm <= fcst_thresh]
      key <- sprintf("%s_%.4f", lt_info$id, rp)
      trigger_lookup[[key]] <- trigger_years
    }
  }

  list(
    trigger_lookup = trigger_lookup,
    primera_ranked = primera_ranked,
    postrera_ranked = postrera_ranked,
    n_years = n_years,
    rp_options = rp_options
  )
}


#' Evaluate all candidate configurations against precomputed trigger data
#'
#' For each config row: look up trigger years per LT, compute OR-union per
#' season, then compute seasonal trigger counts & RPs, matched-RP F1 per
#' season, early warning TPs, marginal FPs, and threshold CV.
#'
#' @param combos tibble with threshold columns (p0, p1, [p2], s0, s1, s2, [s3])
#' @param trigger_data list from build_trigger_lookup()
#' @return combos tibble enriched with all metric columns
#' @param rp_buffer numeric, RP buffer for tolerant F1 (default 1). A trigger
#'   on a year within `seasonal_rp - rp_buffer` is counted as a TP instead of
#'   FP. E.g. if seasonal RP = 4.5 and buffer = 1, years down to RP 3.5 get
#'   TP credit. FN stays the same as matched F1 (no extra penalty).
#' @export
evaluate_configs <- function(combos, trigger_data, rp_buffer = 1) {
  tl <- trigger_data$trigger_lookup
  primera_ranked <- trigger_data$primera_ranked
  postrera_ranked <- trigger_data$postrera_ranked
  n_years <- trigger_data$n_years

  p_cols <- intersect(c("p0", "p1", "p2"), names(combos))
  s_cols <- intersect(c("s0", "s1", "s2", "s3"), names(combos))

  results <- map_dfr(seq_len(nrow(combos)), function(i) {
    row <- combos[i, ]

    # --- Look up trigger years per LT ---
    get_yrs <- function(id, val) {
      if (is.na(val)) return(integer(0))
      key <- sprintf("%s_%.4f", id, val)
      tl[[key]] %||% integer(0)
    }

    p_lt_yrs <- lapply(p_cols, function(col) get_yrs(col, row[[col]]))
    s_lt_yrs <- lapply(s_cols, function(col) get_yrs(col, row[[col]]))

    # Season-level trigger years (OR-union)
    p_trigger_yrs <- Reduce(union, p_lt_yrs, accumulate = FALSE)
    s_trigger_yrs <- Reduce(union, s_lt_yrs, accumulate = FALSE)

    n_p <- length(p_trigger_yrs)
    n_s <- length(s_trigger_yrs)

    # Seasonal and annual RPs (Weibull plotting position: (n+1)/count)
    p_rp <- if (n_p > 0) (n_years + 1) / n_p else Inf
    s_rp <- if (n_s > 0) (n_years + 1) / n_s else Inf
    annual_triggers <- union(p_trigger_yrs, s_trigger_yrs)
    annual_rp <- if (length(annual_triggers) > 0) (n_years + 1) / length(annual_triggers) else Inf

    # Matched drought definition: N driest observed = N trigger years
    p_drought <- head(primera_ranked, n_p)
    s_drought <- head(postrera_ranked, n_s)

    # Confusion matrix per season
    p_tp <- length(intersect(p_trigger_yrs, p_drought))
    p_fp <- length(setdiff(p_trigger_yrs, p_drought))
    p_fn <- length(setdiff(p_drought, p_trigger_yrs))
    s_tp <- length(intersect(s_trigger_yrs, s_drought))
    s_fp <- length(setdiff(s_trigger_yrs, s_drought))
    s_fn <- length(setdiff(s_drought, s_trigger_yrs))

    p_prec <- if (p_tp + p_fp > 0) p_tp / (p_tp + p_fp) else 0
    p_rec <- if (p_tp + p_fn > 0) p_tp / (p_tp + p_fn) else 0
    p_f1 <- if (p_prec + p_rec > 0) 2 * p_prec * p_rec / (p_prec + p_rec) else 0

    s_prec <- if (s_tp + s_fp > 0) s_tp / (s_tp + s_fp) else 0
    s_rec <- if (s_tp + s_fn > 0) s_tp / (s_tp + s_fn) else 0
    s_f1 <- if (s_prec + s_rec > 0) 2 * s_prec * s_rec / (s_prec + s_rec) else 0

    # --- Tolerant F1 (buffer-based, relative to config's own seasonal RP) ---
    # Widens TP credit by rp_buffer: a trigger on a near-miss year (obs RP
    # between seasonal_rp and seasonal_rp - buffer) counts as TP, not FP.
    # FN stays identical to matched F1 (no extra penalty).
    # Guarantees tolerant F1 >= matched F1.
    p_tol_rp <- max(p_rp - rp_buffer, 1)
    p_n_wide <- min(round(n_years / p_tol_rp), n_years)
    p_tol_drought <- head(primera_ranked, p_n_wide)
    p_tol_tp <- length(intersect(p_trigger_yrs, p_tol_drought))
    p_tol_fp <- length(setdiff(p_trigger_yrs, p_tol_drought))
    p_tol_fn <- p_fn  # same as matched
    p_tol_f1 <- if (p_tol_tp + p_tol_fp + p_tol_fn > 0) {
      2 * p_tol_tp / (2 * p_tol_tp + p_tol_fp + p_tol_fn)
    } else 0

    s_tol_rp <- max(s_rp - rp_buffer, 1)
    s_n_wide <- min(round(n_years / s_tol_rp), n_years)
    s_tol_drought <- head(postrera_ranked, s_n_wide)
    s_tol_tp <- length(intersect(s_trigger_yrs, s_tol_drought))
    s_tol_fp <- length(setdiff(s_trigger_yrs, s_tol_drought))
    s_tol_fn <- s_fn  # same as matched
    s_tol_f1 <- if (s_tol_tp + s_tol_fp + s_tol_fn > 0) {
      2 * s_tol_tp / (2 * s_tol_tp + s_tol_fp + s_tol_fn)
    } else 0

    # --- Early warning: TPs first caught at earlier LTs ---
    # Primera: from earliest (highest LT index) to latest (LT0)
    p_yrs_list <- rev(p_lt_yrs)  # earliest first
    p_cumulative <- integer(0)
    p_tp_earliest <- integer(length(p_lt_yrs))
    p_fp_marginal <- integer(length(p_lt_yrs))
    for (j in seq_along(p_yrs_list)) {
      these_yrs <- p_yrs_list[[j]]
      new_tp <- length(setdiff(intersect(these_yrs, p_drought), p_cumulative))
      new_fp <- length(setdiff(setdiff(these_yrs, p_drought), p_cumulative))
      p_tp_earliest[j] <- new_tp
      p_fp_marginal[j] <- new_fp
      p_cumulative <- union(p_cumulative, these_yrs)
    }

    # Postrera: same logic
    s_yrs_list <- rev(s_lt_yrs)
    s_cumulative <- integer(0)
    s_tp_earliest <- integer(length(s_lt_yrs))
    s_fp_marginal <- integer(length(s_lt_yrs))
    for (j in seq_along(s_yrs_list)) {
      these_yrs <- s_yrs_list[[j]]
      new_tp <- length(setdiff(intersect(these_yrs, s_drought), s_cumulative))
      new_fp <- length(setdiff(setdiff(these_yrs, s_drought), s_cumulative))
      s_tp_earliest[j] <- new_tp
      s_fp_marginal[j] <- new_fp
      s_cumulative <- union(s_cumulative, these_yrs)
    }

    # --- Threshold CV per season ---
    p_vals <- unlist(row[p_cols]) |> na.omit()
    s_vals <- unlist(row[s_cols]) |> na.omit()
    p_cv <- if (length(p_vals) >= 2) sd(p_vals) / mean(p_vals) else 0
    s_cv <- if (length(s_vals) >= 2) sd(s_vals) / mean(s_vals) else 0
    all_vals <- c(p_vals, s_vals)
    all_cv <- if (length(all_vals) >= 2) sd(all_vals) / mean(all_vals) else 0

    # Build early warning column names dynamically
    # Primera: reversed order so index 1 = highest LT, last = LT0
    p_ew_names <- paste0("p_tp_earliest_lt", rev(seq_along(p_lt_yrs)) - 1)
    p_fp_names <- paste0("p_fp_marginal_lt", rev(seq_along(p_lt_yrs)) - 1)
    s_ew_names <- paste0("s_tp_earliest_lt", rev(seq_along(s_lt_yrs)) - 1)
    s_fp_names <- paste0("s_fp_marginal_lt", rev(seq_along(s_lt_yrs)) - 1)

    ew_vals <- c(
      setNames(as.list(p_tp_earliest), p_ew_names),
      setNames(as.list(p_fp_marginal), p_fp_names),
      setNames(as.list(s_tp_earliest), s_ew_names),
      setNames(as.list(s_fp_marginal), s_fp_names)
    )

    tibble(
      n_p_triggers = n_p,
      n_s_triggers = n_s,
      p_seasonal_rp = p_rp,
      s_seasonal_rp = s_rp,
      annual_rp = annual_rp,
      primera_tp = p_tp, primera_fp = p_fp, primera_fn = p_fn, primera_f1 = p_f1,
      postrera_tp = s_tp, postrera_fp = s_fp, postrera_fn = s_fn, postrera_f1 = s_f1,
      primera_tol_tp = p_tol_tp, primera_tol_fp = p_tol_fp, primera_tol_fn = p_tol_fn,
      postrera_tol_tp = s_tol_tp, postrera_tol_fp = s_tol_fp, postrera_tol_fn = s_tol_fn,
      primera_tol_f1 = p_tol_f1, postrera_tol_f1 = s_tol_f1,
      mean_tol_f1 = (p_tol_f1 + s_tol_f1) / 2,
      !!!ew_vals,
      mean_f1 = (p_f1 + s_f1) / 2,
      f1_diff = abs(p_f1 - s_f1),
      rp_diff = s_rp - p_rp,
      p_cv = p_cv,
      s_cv = s_cv,
      mean_cv = (p_cv + s_cv) / 2,
      all_cv = all_cv
    )
  })

  # Compute early:MFP ratio
  p_ew_cols <- grep("^p_tp_earliest_lt[^0]", names(results), value = TRUE)
  s_ew_cols <- grep("^s_tp_earliest_lt[^0]", names(results), value = TRUE)
  p_fp_cols <- grep("^p_fp_marginal_lt[^0]", names(results), value = TRUE)
  s_fp_cols <- grep("^s_fp_marginal_lt[^0]", names(results), value = TRUE)

  results <- results |>
    mutate(
      p_early_tp = rowSums(across(any_of(p_ew_cols)), na.rm = TRUE),
      s_early_tp = rowSums(across(any_of(s_ew_cols)), na.rm = TRUE),
      total_early_tp = p_early_tp + s_early_tp,
      p_marginal_fp = rowSums(across(any_of(p_fp_cols)), na.rm = TRUE),
      s_marginal_fp = rowSums(across(any_of(s_fp_cols)), na.rm = TRUE),
      total_marginal_fp = p_marginal_fp + s_marginal_fp
    )

  max_ratio_fp <- results |>
    filter(total_marginal_fp > 0) |>
    summarise(mr = max(total_early_tp / total_marginal_fp, na.rm = TRUE)) |>
    pull(mr)

  if (length(max_ratio_fp) == 0 || is.na(max_ratio_fp)) max_ratio_fp <- 0

  results <- results |>
    mutate(
      early_mfp_ratio = if_else(
        total_marginal_fp > 0,
        total_early_tp / total_marginal_fp,
        max_ratio_fp + total_early_tp
      )
    )

  bind_cols(combos, results) |>
    mutate(config_id = row_number(), .before = 1)
}


#' Filter by annual RP range
#' @param df tibble with annual_rp column
#' @param min numeric, minimum annual RP (inclusive)
#' @param max numeric, maximum annual RP (inclusive)
#' @return filtered tibble
#' @export
filter_annual_rp <- function(df, min, max) {
  df |> filter(annual_rp >= min, annual_rp <= max)
}


#' Filter to configs with annual RP closest to target
#'
#' Keeps the `n` closest unique annual_rp values to `target`, then returns
#' all rows at those RP values. Useful when exact RP matching is too strict.
#'
#' @param df tibble with annual_rp column
#' @param target numeric, target annual RP
#' @param n integer, number of closest unique RP values to keep (default 1)
#' @return filtered tibble
#' @export
filter_closest_annual_rp <- function(df, target, n = 1) {
  unique_rps <- sort(unique(df$annual_rp))
  closest <- unique_rps[order(abs(unique_rps - target))][seq_len(min(n, length(unique_rps)))]
  df |> filter(annual_rp %in% closest)
}


#' Filter by seasonal RP balance
#' @param df tibble with p_seasonal_rp and s_seasonal_rp columns
#' @param max_diff numeric, maximum allowed (postrera_rp - primera_rp)
#' @return filtered tibble
#' @export
filter_rp_balance <- function(df, max_diff) {
  df |> filter(rp_diff >= 0, rp_diff <= max_diff)
}


#' Filter to top F1 configs per LT group
#'
#' Groups by `p_lt_count` + `s_lt_count` (must run add_lt_set() first),
#' keeps the top `n` rows by mean_f1 within each group, preserving ties.
#'
#' @param df tibble with mean_f1, p_lt_count, s_lt_count columns
#' @param n integer, number of top rows to keep per group (default 1).
#'   All ties at the nth value are kept.
#' @return filtered tibble
#' @export
filter_top_f1 <- function(df, n = 1) {
  df |>
    group_by(p_lt_count, s_lt_count) |>
    slice_max(mean_f1, n = n, with_ties = TRUE) |>
    ungroup()
}


#' Add must-hit and should-hit coverage scores
#'
#' For each config, computes the fraction of must-hit and should-hit years
#' that are covered by the config's combined trigger years (primera OR postrera).
#' Must-hit years = severe regional droughts (all 3 countries in EM-DAT).
#' Should-hit years = moderate regional droughts (2+ countries).
#'
#' @param df tibble from evaluate_configs() or evaluate_configs_impact(),
#'   must include threshold columns (p0, p1, ..., s0, s1, ...)
#' @param trigger_data list from build_trigger_lookup()
#' @param must_hit_years integer vector of must-hit years (3/3 countries)
#' @param should_hit_years integer vector of should-hit years (2+/3 countries)
#' @param eval_start integer, first year of evaluation window (default 1991)
#' @param eval_end integer, last year of evaluation window (default 2024)
#' @return df with added `must_hit_score` (0-1) and `should_hit_score` (0-1)
#' @export
add_hit_scores <- function(df, trigger_data, must_hit_years,
                           should_hit_years,
                           eval_start = 1991, eval_end = 2024) {
  tl <- trigger_data$trigger_lookup
  eval_window <- eval_start:eval_end
  must_hit <- intersect(must_hit_years, eval_window)
  should_hit <- intersect(should_hit_years, eval_window)

  p_cols <- intersect(c("p0", "p1", "p2"), names(df))
  s_cols <- intersect(c("s0", "s1", "s2", "s3"), names(df))

  scores <- t(vapply(seq_len(nrow(df)), function(i) {
    row <- df[i, ]

    get_yrs <- function(id, val) {
      if (is.na(val)) return(integer(0))
      key <- sprintf("%s_%.4f", id, val)
      yrs <- tl[[key]] %||% integer(0)
      intersect(yrs, eval_window)
    }

    p_yrs <- Reduce(union, lapply(p_cols, function(col) get_yrs(col, row[[col]])))
    s_yrs <- Reduce(union, lapply(s_cols, function(col) get_yrs(col, row[[col]])))
    all_trigger_yrs <- union(p_yrs, s_yrs)

    mh <- if (length(must_hit) > 0) sum(must_hit %in% all_trigger_yrs) / length(must_hit) else 1
    sh <- if (length(should_hit) > 0) sum(should_hit %in% all_trigger_yrs) / length(should_hit) else 1
    c(mh, sh)
  }, numeric(2)))

  df |>
    mutate(
      must_hit_score = scores[, 1],
      should_hit_score = scores[, 2]
    )
}


#' Add a column classifying each config's active leadtime set
#'
#' Adds `p_lt_count` and `s_lt_count` columns counting how many
#' leadtimes are active (non-NA) per season.
#'
#' @param df tibble with threshold columns (p0, p1, ..., s0, s1, ...)
#' @return df with added `p_lt_count`, `s_lt_count` columns
#' @export
add_lt_set <- function(df) {
  p_cols <- intersect(c("p0", "p1", "p2"), names(df))
  s_cols <- intersect(c("s0", "s1", "s2", "s3"), names(df))

  df |>
    mutate(
      p_lt_count = rowSums(!is.na(across(all_of(p_cols)))),
      s_lt_count = rowSums(!is.na(across(all_of(s_cols))))
    )
}


#' Rank and select top candidates within each LT group
#'
#' Groups by `p_lt_count` + `s_lt_count`, ranks within each group by:
#' lowest mean_cv, then fewest total_marginal_fp, then highest
#' early_mfp_ratio. Returns top `n` per group. Run filter_top_f1()
#' first to pre-filter on F1.
#'
#' @param df tibble with `p_lt_count`, `s_lt_count` columns (from add_lt_set())
#' @param n integer, number of candidates per LT group (default 3)
#' @return tibble with top `n` candidates per LT group
#' @export
rank_within_lt_set <- function(df, n = 3) {
  df |>
    group_by(p_lt_count, s_lt_count) |>
    arrange(mean_cv, total_marginal_fp, desc(early_mfp_ratio),
            .by_group = TRUE) |>
    slice_head(n = n) |>
    ungroup() |>
    arrange(p_lt_count, s_lt_count, mean_cv, total_marginal_fp, desc(early_mfp_ratio))
}


#' Get season-level trigger years for a single config
#'
#' Lightweight helper that returns the OR-union of trigger years per season.
#' Used internally by `summarise_archetypes()` to check year agreement.
#'
#' @param config single-row tibble with threshold columns
#' @param trigger_data list from build_trigger_lookup()
#' @return list with `primera` and `postrera` integer vectors of trigger years
#' @export
get_trigger_years <- function(config, trigger_data) {
  tl <- trigger_data$trigger_lookup
  p_cols <- intersect(c("p0", "p1", "p2"), names(config))
  s_cols <- intersect(c("s0", "s1", "s2", "s3"), names(config))

  get_yrs <- function(id, val) {
    if (is.na(val)) return(integer(0))
    key <- sprintf("%s_%.4f", id, val)
    tl[[key]] %||% integer(0)
  }

  p_yrs <- lapply(p_cols, function(col) get_yrs(col, config[[col]]))
  s_yrs <- lapply(s_cols, function(col) get_yrs(col, config[[col]]))

  list(
    primera = sort(Reduce(union, p_yrs, accumulate = FALSE)),
    postrera = sort(Reduce(union, s_yrs, accumulate = FALSE))
  )
}


#' Recommend a single config from evaluated results
#'
#' Applies opinionated heuristics to select one threshold set:
#' 1. Filter to target LT set and RP balance
#' 2. Maximize must_hit_score, then should_hit_score (if columns present)
#' 3. Among top hit-score configs: best mean F1 at each RP
#' 4. Pick archetype closest to target RP with highest F1
#' 5. Within archetype: prefer flat (CV=0) > lowest CV > fewest marginal FPs
#'
#' Returns the single config row with a text rationale.
#'
#' @param df tibble from evaluate_configs() (must have add_lt_set() applied).
#'   If must_hit_score/should_hit_score columns are present (from add_hit_scores()),
#'   they are used as priority criteria before F1 selection.
#' @param target_rp numeric, target annual return period
#' @param p_lt integer, number of primera leadtimes to use
#' @param s_lt integer, number of postrera leadtimes to use
#' @param max_rp_balance numeric, max allowed postrera_rp - primera_rp (default 2)
#' @return list with `config` (single-row tibble), `rationale` (character string),
#'   `alternatives` (tibble of other configs in the same archetype)
#' @export
recommend_config <- function(df, target_rp, p_lt, s_lt, max_rp_balance = 2) {
  candidates <- df |>
    filter(p_lt_count == p_lt, s_lt_count == s_lt) |>
    filter_rp_balance(max_diff = max_rp_balance)

  if (nrow(candidates) == 0) {
    stop("No configs match the LT set and RP balance constraints.")
  }

  # Priority filtering if hit scores present:
  # 1. Hard gate: must hit ALL priority 1 years (must_hit_score == 1)
  # 2. Among those: keep configs with max priority 2 hits (should_hit_score)
  # Fallback: if no config hits all priority 1, keep max must_hit_score
  has_hit_scores <- all(c("must_hit_score", "should_hit_score") %in% names(candidates))
  hit_filtered <- FALSE
  if (has_hit_scores) {
    perfect_must <- candidates |> filter(must_hit_score == 1)
    if (nrow(perfect_must) > 0) {
      candidates <- perfect_must
      hit_filtered <- TRUE
    } else {
      best_mh <- max(candidates$must_hit_score)
      candidates <- candidates |> filter(must_hit_score == best_mh)
      warning(sprintf(
        "No configs hit all priority 1 years. Best must_hit_score = %.0f%%.",
        best_mh * 100
      ))
    }
    best_sh <- max(candidates$should_hit_score)
    candidates <- candidates |> filter(should_hit_score == best_sh)
  }

  # Best F1 at each RP level
  top_f1 <- candidates |>
    group_by(annual_rp) |>
    filter(mean_f1 == max(mean_f1)) |>
    ungroup()

  # Pick RP closest to target
  unique_rps <- sort(unique(top_f1$annual_rp))
  best_rp <- unique_rps[which.min(abs(unique_rps - target_rp))]
  archetype <- top_f1 |> filter(annual_rp == best_rp)

  # Within archetype: prefer flat (CV=0), then lowest CV, then fewest FPs
  ranked <- archetype |>
    arrange(mean_cv, total_marginal_fp, desc(early_mfp_ratio))

  pick <- ranked[1, ]
  n_alt <- nrow(ranked) - 1

  # Build rationale
  p_cols <- intersect(c("p0", "p1", "p2"), names(pick))
  s_cols <- intersect(c("s0", "s1", "s2", "s3"), names(pick))
  fmt <- function(x) if (is.na(x)) "\u2014" else sprintf("%.2f", x)

  hit_line <- ""
  if (has_hit_scores) {
    hit_line <- sprintf(
      "  Priority 1 (must-hit): %.0f%% | Priority 2 (should-hit): %.0f%%%s\n",
      pick$must_hit_score * 100, pick$should_hit_score * 100,
      if (hit_filtered) " [hard-filtered to 100% P1]" else " [fallback: best available]"
    )
  }

  rationale <- paste0(
    "Selected config_id ", pick$config_id, ":\n",
    "  Annual RP: ", sprintf("%.2f", pick$annual_rp),
    " (target: ", sprintf("%.2f", target_rp),
    ", delta: ", sprintf("%+.2f", pick$annual_rp - target_rp), ")\n",
    "  Seasonal RPs: primera=", sprintf("%.2f", pick$p_seasonal_rp),
    ", postrera=", sprintf("%.2f", pick$s_seasonal_rp), "\n",
    "  Mean F1: ", sprintf("%.3f", pick$mean_f1),
    " (pri=", sprintf("%.3f", pick$primera_f1),
    ", post=", sprintf("%.3f", pick$postrera_f1), ")\n",
    hit_line,
    "  Thresholds (RP): primera=[",
    paste(sapply(pick[p_cols], fmt), collapse = ", "), "], postrera=[",
    paste(sapply(pick[s_cols], fmt), collapse = ", "), "]\n",
    "  Tolerant F1: ", sprintf("%.3f", pick$mean_tol_f1),
    " (pri=", sprintf("%.3f", pick$primera_tol_f1),
    ", post=", sprintf("%.3f", pick$postrera_tol_f1), ")\n",
    "  CV: ", sprintf("%.3f", pick$mean_cv),
    if (pick$mean_cv == 0) " (flat — same threshold all LTs)" else "", "\n",
    "  ", n_alt, " alternative config(s) with identical F1/RP in same archetype."
  )

  list(config = pick, rationale = rationale, alternatives = ranked[-1, ])
}


#' Build per-year, per-leadtime classification for a single config
#'
#' For a given config, classifies each year × leadtime as:
#' TP_first, TP_redundant, FP_marginal, FP_redundant, FN, TN.
#' Returns tidy data suitable for gt or ggplot visualization.
#'
#' @param config single-row tibble with threshold columns (p0, p1, ..., s0, s1, ...)
#' @param trigger_data list from build_trigger_lookup()
#' @return tibble with columns: year, season, lt_id, status
#' @export
build_config_detail <- function(config, trigger_data) {
  tl <- trigger_data$trigger_lookup
  primera_ranked <- trigger_data$primera_ranked
  postrera_ranked <- trigger_data$postrera_ranked
  n_years <- trigger_data$n_years
  all_years <- seq(min(primera_ranked, postrera_ranked),
                   max(primera_ranked, postrera_ranked))

  p_cols <- intersect(c("p0", "p1", "p2"), names(config))
  s_cols <- intersect(c("s0", "s1", "s2", "s3"), names(config))

  get_yrs <- function(id, val) {
    if (is.na(val)) return(NULL)
    key <- sprintf("%s_%.4f", id, val)
    tl[[key]] %||% integer(0)
  }

  # Collect per-LT trigger years (only active LTs)
  p_lt_yrs <- list()
  for (col in p_cols) {
    yrs <- get_yrs(col, config[[col]])
    if (!is.null(yrs)) p_lt_yrs[[col]] <- yrs
  }
  s_lt_yrs <- list()
  for (col in s_cols) {
    yrs <- get_yrs(col, config[[col]])
    if (!is.null(yrs)) s_lt_yrs[[col]] <- yrs
  }

  # Season-level unions
  p_trigger <- Reduce(union, p_lt_yrs, accumulate = FALSE)
  s_trigger <- Reduce(union, s_lt_yrs, accumulate = FALSE)

  # Matched drought
  p_drought <- head(primera_ranked, length(p_trigger))
  s_drought <- head(postrera_ranked, length(s_trigger))

  # Classify one season's LTs
  classify_season <- function(lt_yrs_list, drought_yrs, season_name) {
    # Process from earliest (highest LT index) to latest (LT0)
    lt_ids <- rev(names(lt_yrs_list))
    cumulative <- integer(0)

    rows <- list()
    for (lt_id in lt_ids) {
      these_yrs <- lt_yrs_list[[lt_id]]
      for (yr in all_years) {
        triggered <- yr %in% these_yrs
        is_drought <- yr %in% drought_yrs
        in_later <- yr %in% cumulative
        status <- if (triggered && is_drought && !in_later) {
          "TP_first"
        } else if (triggered && is_drought && in_later) {
          "TP_redundant"
        } else if (triggered && !is_drought && !in_later) {
          "FP_marginal"
        } else if (triggered && !is_drought && in_later) {
          "FP_redundant"
        } else if (!triggered && is_drought) {
          "FN"
        } else {
          "TN"
        }
        rows <- c(rows, list(list(
          year = yr, season = season_name, lt_id = lt_id, status = status
        )))
      }
      cumulative <- union(cumulative, these_yrs)
    }
    rows
  }

  all_rows <- c(
    classify_season(p_lt_yrs, p_drought, "primera"),
    classify_season(s_lt_yrs, s_drought, "postrera")
  )

  map_dfr(all_rows, as_tibble)
}


#' Evaluate configs against fixed impact years (e.g. EM-DAT)
#'
#' Same grid search input and output schema as `evaluate_configs()`, but
#' evaluates against fixed impact years instead of ERA5 matched-RP drought.
#' F1 is standard (not matched-RP): TP = trigger AND impact year.
#' Impact years are duplicated to both seasons (EM-DAT is annual).
#'
#' @param combos tibble with threshold columns (p0, p1, [p2], s0, s1, s2, [s3])
#' @param trigger_data list from build_trigger_lookup()
#' @param impact_years integer vector of impact event years (e.g. from EM-DAT)
#' @param eval_start integer, first year of evaluation window (default 1991)
#' @param eval_end integer, last year of evaluation window (default 2024)
#' @return combos tibble enriched with all metric columns (same schema as
#'   evaluate_configs output)
#' @export
evaluate_configs_impact <- function(combos, trigger_data,
                                     impact_years,
                                     eval_start = 1991,
                                     eval_end = 2024) {
  tl <- trigger_data$trigger_lookup
  n_years <- eval_end - eval_start + 1
  eval_window <- eval_start:eval_end

  # Fixed drought = impact years within evaluation window (same for both seasons)
  impact_yrs <- sort(intersect(impact_years, eval_window))

  p_cols <- intersect(c("p0", "p1", "p2"), names(combos))
  s_cols <- intersect(c("s0", "s1", "s2", "s3"), names(combos))

  results <- map_dfr(seq_len(nrow(combos)), function(i) {
    row <- combos[i, ]

    # --- Look up trigger years per LT, filtered to eval window ---
    get_yrs <- function(id, val) {
      if (is.na(val)) return(integer(0))
      key <- sprintf("%s_%.4f", id, val)
      yrs <- tl[[key]] %||% integer(0)
      intersect(yrs, eval_window)
    }

    p_lt_yrs <- lapply(p_cols, function(col) get_yrs(col, row[[col]]))
    s_lt_yrs <- lapply(s_cols, function(col) get_yrs(col, row[[col]]))

    # Season-level trigger years (OR-union)
    p_trigger_yrs <- Reduce(union, p_lt_yrs, accumulate = FALSE)
    s_trigger_yrs <- Reduce(union, s_lt_yrs, accumulate = FALSE)

    n_p <- length(p_trigger_yrs)
    n_s <- length(s_trigger_yrs)

    # Seasonal and annual RPs (Weibull plotting position: (n+1)/count)
    p_rp <- if (n_p > 0) (n_years + 1) / n_p else Inf
    s_rp <- if (n_s > 0) (n_years + 1) / n_s else Inf
    annual_triggers <- intersect(union(p_trigger_yrs, s_trigger_yrs), eval_window)
    annual_rp <- if (length(annual_triggers) > 0) (n_years + 1) / length(annual_triggers) else Inf

    # --- Standard F1 against fixed impact years ---
    p_tp <- length(intersect(p_trigger_yrs, impact_yrs))
    p_fp <- length(setdiff(p_trigger_yrs, impact_yrs))
    p_fn <- length(setdiff(impact_yrs, p_trigger_yrs))
    s_tp <- length(intersect(s_trigger_yrs, impact_yrs))
    s_fp <- length(setdiff(s_trigger_yrs, impact_yrs))
    s_fn <- length(setdiff(impact_yrs, s_trigger_yrs))

    p_prec <- if (p_tp + p_fp > 0) p_tp / (p_tp + p_fp) else 0
    p_rec <- if (p_tp + p_fn > 0) p_tp / (p_tp + p_fn) else 0
    p_f1 <- if (p_prec + p_rec > 0) 2 * p_prec * p_rec / (p_prec + p_rec) else 0

    s_prec <- if (s_tp + s_fp > 0) s_tp / (s_tp + s_fp) else 0
    s_rec <- if (s_tp + s_fn > 0) s_tp / (s_tp + s_fn) else 0
    s_f1 <- if (s_prec + s_rec > 0) 2 * s_prec * s_rec / (s_prec + s_rec) else 0

    # --- Early warning: TPs first caught at earlier LTs ---
    # Classified against impact_years instead of ERA5 ranked
    p_yrs_list <- rev(p_lt_yrs)
    p_cumulative <- integer(0)
    p_tp_earliest <- integer(length(p_lt_yrs))
    p_fp_marginal <- integer(length(p_lt_yrs))
    for (j in seq_along(p_yrs_list)) {
      these_yrs <- p_yrs_list[[j]]
      new_tp <- length(setdiff(intersect(these_yrs, impact_yrs), p_cumulative))
      new_fp <- length(setdiff(setdiff(these_yrs, impact_yrs), p_cumulative))
      p_tp_earliest[j] <- new_tp
      p_fp_marginal[j] <- new_fp
      p_cumulative <- union(p_cumulative, these_yrs)
    }

    s_yrs_list <- rev(s_lt_yrs)
    s_cumulative <- integer(0)
    s_tp_earliest <- integer(length(s_lt_yrs))
    s_fp_marginal <- integer(length(s_lt_yrs))
    for (j in seq_along(s_yrs_list)) {
      these_yrs <- s_yrs_list[[j]]
      new_tp <- length(setdiff(intersect(these_yrs, impact_yrs), s_cumulative))
      new_fp <- length(setdiff(setdiff(these_yrs, impact_yrs), s_cumulative))
      s_tp_earliest[j] <- new_tp
      s_fp_marginal[j] <- new_fp
      s_cumulative <- union(s_cumulative, these_yrs)
    }

    # --- Threshold CV per season ---
    p_vals <- unlist(row[p_cols]) |> na.omit()
    s_vals <- unlist(row[s_cols]) |> na.omit()
    p_cv <- if (length(p_vals) >= 2) sd(p_vals) / mean(p_vals) else 0
    s_cv <- if (length(s_vals) >= 2) sd(s_vals) / mean(s_vals) else 0
    all_vals <- c(p_vals, s_vals)
    all_cv <- if (length(all_vals) >= 2) sd(all_vals) / mean(all_vals) else 0

    # Build early warning column names dynamically
    p_ew_names <- paste0("p_tp_earliest_lt", rev(seq_along(p_lt_yrs)) - 1)
    p_fp_names <- paste0("p_fp_marginal_lt", rev(seq_along(p_lt_yrs)) - 1)
    s_ew_names <- paste0("s_tp_earliest_lt", rev(seq_along(s_lt_yrs)) - 1)
    s_fp_names <- paste0("s_fp_marginal_lt", rev(seq_along(s_lt_yrs)) - 1)

    ew_vals <- c(
      setNames(as.list(p_tp_earliest), p_ew_names),
      setNames(as.list(p_fp_marginal), p_fp_names),
      setNames(as.list(s_tp_earliest), s_ew_names),
      setNames(as.list(s_fp_marginal), s_fp_names)
    )

    tibble(
      n_p_triggers = n_p,
      n_s_triggers = n_s,
      p_seasonal_rp = p_rp,
      s_seasonal_rp = s_rp,
      annual_rp = annual_rp,
      primera_tp = p_tp, primera_fp = p_fp, primera_fn = p_fn, primera_f1 = p_f1,
      postrera_tp = s_tp, postrera_fp = s_fp, postrera_fn = s_fn, postrera_f1 = s_f1,
      primera_tol_f1 = p_f1, postrera_tol_f1 = s_f1,
      mean_tol_f1 = (p_f1 + s_f1) / 2,
      !!!ew_vals,
      mean_f1 = (p_f1 + s_f1) / 2,
      f1_diff = abs(p_f1 - s_f1),
      rp_diff = s_rp - p_rp,
      p_cv = p_cv,
      s_cv = s_cv,
      mean_cv = (p_cv + s_cv) / 2,
      all_cv = all_cv
    )
  })

  # Compute early:MFP ratio (same post-processing as evaluate_configs)
  p_ew_cols <- grep("^p_tp_earliest_lt[^0]", names(results), value = TRUE)
  s_ew_cols <- grep("^s_tp_earliest_lt[^0]", names(results), value = TRUE)
  p_fp_cols <- grep("^p_fp_marginal_lt[^0]", names(results), value = TRUE)
  s_fp_cols <- grep("^s_fp_marginal_lt[^0]", names(results), value = TRUE)

  results <- results |>
    mutate(
      p_early_tp = rowSums(across(any_of(p_ew_cols)), na.rm = TRUE),
      s_early_tp = rowSums(across(any_of(s_ew_cols)), na.rm = TRUE),
      total_early_tp = p_early_tp + s_early_tp,
      p_marginal_fp = rowSums(across(any_of(p_fp_cols)), na.rm = TRUE),
      s_marginal_fp = rowSums(across(any_of(s_fp_cols)), na.rm = TRUE),
      total_marginal_fp = p_marginal_fp + s_marginal_fp
    )

  max_ratio_fp <- results |>
    filter(total_marginal_fp > 0) |>
    summarise(mr = max(total_early_tp / total_marginal_fp, na.rm = TRUE)) |>
    pull(mr)

  if (length(max_ratio_fp) == 0 || is.na(max_ratio_fp)) max_ratio_fp <- 0

  results <- results |>
    mutate(
      early_mfp_ratio = if_else(
        total_marginal_fp > 0,
        total_early_tp / total_marginal_fp,
        max_ratio_fp + total_early_tp
      )
    )

  bind_cols(combos, results) |>
    mutate(config_id = row_number(), .before = 1)
}


#' Evaluate configs against impact years using OR-union of two independent AOIs
#'
#' Same grid search as evaluate_configs_impact(), but each config's thresholds
#' are applied independently to two separate AOI trigger lookups. A season
#' triggers if *either* AOI triggers (OR logic). The annual RP reflects this
#' union. Output schema is identical to evaluate_configs_impact() so all
#' downstream viz and filtering functions work unchanged.
#'
#' @param combos tibble with threshold columns (p0, p1, ..., s0, s1, ...)
#' @param trigger_data_1 list from build_trigger_lookup() for AOI 1
#' @param trigger_data_2 list from build_trigger_lookup() for AOI 2
#' @param impact_years integer vector of EM-DAT drought years
#' @param eval_start start year of evaluation window (default 1991)
#' @param eval_end end year of evaluation window (default 2024)
#' @return combos tibble enriched with all metric columns (same schema as
#'   evaluate_configs_impact)
#' @export
evaluate_configs_impact_or <- function(combos,
                                       trigger_data_1,
                                       trigger_data_2,
                                       impact_years,
                                       eval_start = 1991,
                                       eval_end = 2024) {
  tl1 <- trigger_data_1$trigger_lookup
  tl2 <- trigger_data_2$trigger_lookup
  n_years <- eval_end - eval_start + 1
  eval_window <- eval_start:eval_end

  impact_yrs <- sort(intersect(impact_years, eval_window))

  p_cols <- intersect(c("p0", "p1", "p2"), names(combos))
  s_cols <- intersect(c("s0", "s1", "s2", "s3"), names(combos))

  results <- map_dfr(seq_len(nrow(combos)), function(i) {
    row <- combos[i, ]

    # --- Look up trigger years per LT from EACH AOI, then OR-union ---
    get_yrs <- function(tl, id, val) {
      if (is.na(val)) return(integer(0))
      key <- sprintf("%s_%.4f", id, val)
      yrs <- tl[[key]] %||% integer(0)
      intersect(yrs, eval_window)
    }

    # Per-LT trigger years: union across AOIs first, then OR across LTs
    p_lt_yrs <- lapply(p_cols, function(col) {
      union(
        get_yrs(tl1, col, row[[col]]),
        get_yrs(tl2, col, row[[col]])
      )
    })
    s_lt_yrs <- lapply(s_cols, function(col) {
      union(
        get_yrs(tl1, col, row[[col]]),
        get_yrs(tl2, col, row[[col]])
      )
    })

    # Season-level trigger years (OR-union across LTs)
    p_trigger_yrs <- Reduce(union, p_lt_yrs, accumulate = FALSE)
    s_trigger_yrs <- Reduce(union, s_lt_yrs, accumulate = FALSE)

    n_p <- length(p_trigger_yrs)
    n_s <- length(s_trigger_yrs)

    # Seasonal and annual RPs
    p_rp <- if (n_p > 0) (n_years + 1) / n_p else Inf
    s_rp <- if (n_s > 0) (n_years + 1) / n_s else Inf
    annual_triggers <- intersect(union(p_trigger_yrs, s_trigger_yrs), eval_window)
    annual_rp <- if (length(annual_triggers) > 0) (n_years + 1) / length(annual_triggers) else Inf

    # --- Standard F1 against fixed impact years ---
    p_tp <- length(intersect(p_trigger_yrs, impact_yrs))
    p_fp <- length(setdiff(p_trigger_yrs, impact_yrs))
    p_fn <- length(setdiff(impact_yrs, p_trigger_yrs))
    s_tp <- length(intersect(s_trigger_yrs, impact_yrs))
    s_fp <- length(setdiff(s_trigger_yrs, impact_yrs))
    s_fn <- length(setdiff(impact_yrs, s_trigger_yrs))

    p_prec <- if (p_tp + p_fp > 0) p_tp / (p_tp + p_fp) else 0
    p_rec <- if (p_tp + p_fn > 0) p_tp / (p_tp + p_fn) else 0
    p_f1 <- if (p_prec + p_rec > 0) 2 * p_prec * p_rec / (p_prec + p_rec) else 0

    s_prec <- if (s_tp + s_fp > 0) s_tp / (s_tp + s_fp) else 0
    s_rec <- if (s_tp + s_fn > 0) s_tp / (s_tp + s_fn) else 0
    s_f1 <- if (s_prec + s_rec > 0) 2 * s_prec * s_rec / (s_prec + s_rec) else 0

    # --- Early warning: TPs first caught at earlier LTs ---
    p_yrs_list <- rev(p_lt_yrs)
    p_cumulative <- integer(0)
    p_tp_earliest <- integer(length(p_lt_yrs))
    p_fp_marginal <- integer(length(p_lt_yrs))
    for (j in seq_along(p_yrs_list)) {
      these_yrs <- p_yrs_list[[j]]
      new_tp <- length(setdiff(intersect(these_yrs, impact_yrs), p_cumulative))
      new_fp <- length(setdiff(setdiff(these_yrs, impact_yrs), p_cumulative))
      p_tp_earliest[j] <- new_tp
      p_fp_marginal[j] <- new_fp
      p_cumulative <- union(p_cumulative, these_yrs)
    }

    s_yrs_list <- rev(s_lt_yrs)
    s_cumulative <- integer(0)
    s_tp_earliest <- integer(length(s_lt_yrs))
    s_fp_marginal <- integer(length(s_lt_yrs))
    for (j in seq_along(s_yrs_list)) {
      these_yrs <- s_yrs_list[[j]]
      new_tp <- length(setdiff(intersect(these_yrs, impact_yrs), s_cumulative))
      new_fp <- length(setdiff(setdiff(these_yrs, impact_yrs), s_cumulative))
      s_tp_earliest[j] <- new_tp
      s_fp_marginal[j] <- new_fp
      s_cumulative <- union(s_cumulative, these_yrs)
    }

    # --- Threshold CV per season ---
    p_vals <- unlist(row[p_cols]) |> na.omit()
    s_vals <- unlist(row[s_cols]) |> na.omit()
    p_cv <- if (length(p_vals) >= 2) sd(p_vals) / mean(p_vals) else 0
    s_cv <- if (length(s_vals) >= 2) sd(s_vals) / mean(s_vals) else 0
    all_vals <- c(p_vals, s_vals)
    all_cv <- if (length(all_vals) >= 2) sd(all_vals) / mean(all_vals) else 0

    # Build early warning column names dynamically
    p_ew_names <- paste0("p_tp_earliest_lt", rev(seq_along(p_lt_yrs)) - 1)
    p_fp_names <- paste0("p_fp_marginal_lt", rev(seq_along(p_lt_yrs)) - 1)
    s_ew_names <- paste0("s_tp_earliest_lt", rev(seq_along(s_lt_yrs)) - 1)
    s_fp_names <- paste0("s_fp_marginal_lt", rev(seq_along(s_lt_yrs)) - 1)

    ew_vals <- c(
      setNames(as.list(p_tp_earliest), p_ew_names),
      setNames(as.list(p_fp_marginal), p_fp_names),
      setNames(as.list(s_tp_earliest), s_ew_names),
      setNames(as.list(s_fp_marginal), s_fp_names)
    )

    tibble(
      n_p_triggers = n_p,
      n_s_triggers = n_s,
      p_seasonal_rp = p_rp,
      s_seasonal_rp = s_rp,
      annual_rp = annual_rp,
      primera_tp = p_tp, primera_fp = p_fp, primera_fn = p_fn, primera_f1 = p_f1,
      postrera_tp = s_tp, postrera_fp = s_fp, postrera_fn = s_fn, postrera_f1 = s_f1,
      primera_tol_f1 = p_f1, postrera_tol_f1 = s_f1,
      mean_tol_f1 = (p_f1 + s_f1) / 2,
      !!!ew_vals,
      mean_f1 = (p_f1 + s_f1) / 2,
      f1_diff = abs(p_f1 - s_f1),
      rp_diff = s_rp - p_rp,
      p_cv = p_cv,
      s_cv = s_cv,
      mean_cv = (p_cv + s_cv) / 2,
      all_cv = all_cv
    )
  })

  # Compute early:MFP ratio (OR variant)
  p_ew_cols <- grep("^p_tp_earliest_lt[^0]", names(results), value = TRUE)
  s_ew_cols <- grep("^s_tp_earliest_lt[^0]", names(results), value = TRUE)
  p_fp_cols <- grep("^p_fp_marginal_lt[^0]", names(results), value = TRUE)
  s_fp_cols <- grep("^s_fp_marginal_lt[^0]", names(results), value = TRUE)

  results <- results |>
    mutate(
      p_early_tp = rowSums(across(any_of(p_ew_cols)), na.rm = TRUE),
      s_early_tp = rowSums(across(any_of(s_ew_cols)), na.rm = TRUE),
      total_early_tp = p_early_tp + s_early_tp,
      p_marginal_fp = rowSums(across(any_of(p_fp_cols)), na.rm = TRUE),
      s_marginal_fp = rowSums(across(any_of(s_fp_cols)), na.rm = TRUE),
      total_marginal_fp = p_marginal_fp + s_marginal_fp
    )

  max_ratio_fp <- results |>
    filter(total_marginal_fp > 0) |>
    summarise(mr = max(total_early_tp / total_marginal_fp, na.rm = TRUE)) |>
    pull(mr)

  if (length(max_ratio_fp) == 0 || is.na(max_ratio_fp)) max_ratio_fp <- 0

  results <- results |>
    mutate(
      early_mfp_ratio = if_else(
        total_marginal_fp > 0,
        total_early_tp / total_marginal_fp,
        max_ratio_fp + total_early_tp
      )
    )

  bind_cols(combos, results) |>
    mutate(config_id = row_number(), .before = 1)
}


#' Merge two trigger lookups with OR logic
#'
#' Takes two trigger_data lists (from build_trigger_lookup()) and returns a
#' new one where each key's trigger years are the union. This makes the merged
#' lookup compatible with all existing downstream functions that expect a single
#' trigger_data (build_trigger_activation_gt, get_seasonal_activation,
#' add_hit_scores, evaluate_configs, etc.).
#'
#' primera_ranked / postrera_ranked are taken from trigger_data_1 by default
#' (used for ERA5 drought year labeling — no single "correct" ranking when
#' AOIs are independent, but needed to keep the struct valid).
#'
#' @param trigger_data_1 list from build_trigger_lookup() for AOI 1
#' @param trigger_data_2 list from build_trigger_lookup() for AOI 2
#' @return merged trigger_data list with same structure
#' @export
merge_trigger_lookups_or <- function(trigger_data_1, trigger_data_2) {
  tl1 <- trigger_data_1$trigger_lookup
  tl2 <- trigger_data_2$trigger_lookup

  all_keys <- union(names(tl1), names(tl2))

  merged_tl <- setNames(
    lapply(all_keys, function(k) {
      union(tl1[[k]] %||% integer(0), tl2[[k]] %||% integer(0))
    }),
    all_keys
  )

  # Merge rp_options: union of RP values per LT id
  rp1 <- trigger_data_1$rp_options
  rp2 <- trigger_data_2$rp_options
  all_lt_ids <- union(names(rp1), names(rp2))
  merged_rp <- setNames(
    lapply(all_lt_ids, function(id) {
      sort(union(rp1[[id]] %||% numeric(0), rp2[[id]] %||% numeric(0)))
    }),
    all_lt_ids
  )

  list(
    trigger_lookup = merged_tl,
    primera_ranked = trigger_data_1$primera_ranked,
    postrera_ranked = trigger_data_1$postrera_ranked,
    n_years = trigger_data_1$n_years,
    rp_options = merged_rp
  )
}


#' Get per-AOI seasonal activation for an OR trigger config
#'
#' Calls the trigger lookup for each AOI separately, returning which years
#' each AOI triggers (primera/postrera) alongside the OR union. Useful for
#' activation summary tables that need to show which AOI(s) fired.
#'
#' @param config_row single-row tibble/data.frame with threshold columns
#' @param trigger_data_1 list from build_trigger_lookup() for AOI 1
#' @param trigger_data_2 list from build_trigger_lookup() for AOI 2
#' @param eval_start start year of evaluation window
#' @param eval_end end year of evaluation window
#' @param aoi1_label character label for AOI 1 (default "AOI1")
#' @param aoi2_label character label for AOI 2 (default "AOI2")
#' @return list with: aoi1_primera, aoi1_postrera, aoi2_primera, aoi2_postrera,
#'   primera (union), postrera (union)
#' @export
get_seasonal_activation_or <- function(config_row,
                                        trigger_data_1,
                                        trigger_data_2,
                                        eval_start, eval_end,
                                        aoi1_label = "AOI1",
                                        aoi2_label = "AOI2") {
  all_lts <- c("p0", "p1", "p2", "s0", "s1", "s2", "s3")
  active_lts <- all_lts[all_lts %in% names(config_row) &
                           !is.na(unlist(config_row[all_lts[all_lts %in% names(config_row)]]))]

  p_lts <- active_lts[startsWith(active_lts, "p")]
  s_lts <- active_lts[startsWith(active_lts, "s")]

  get_trigger_yrs <- function(tl, lts) {
    yrs <- integer(0)
    for (col in lts) {
      key <- sprintf("%s_%.4f", col, config_row[[col]])
      y <- tl[[key]]
      if (!is.null(y)) yrs <- union(yrs, y)
    }
    yrs[yrs >= eval_start & yrs <= eval_end]
  }

  tl1 <- trigger_data_1$trigger_lookup
  tl2 <- trigger_data_2$trigger_lookup

  result <- list()
  result[[paste0(aoi1_label, "_primera")]]  <- get_trigger_yrs(tl1, p_lts)
  result[[paste0(aoi1_label, "_postrera")]] <- get_trigger_yrs(tl1, s_lts)
  result[[paste0(aoi2_label, "_primera")]]  <- get_trigger_yrs(tl2, p_lts)
  result[[paste0(aoi2_label, "_postrera")]] <- get_trigger_yrs(tl2, s_lts)
  result$primera  <- union(result[[paste0(aoi1_label, "_primera")]],
                            result[[paste0(aoi2_label, "_primera")]])
  result$postrera <- union(result[[paste0(aoi1_label, "_postrera")]],
                            result[[paste0(aoi2_label, "_postrera")]])

  result
}


#' Get per-AOI seasonal activation for a paired OR config (different thresholds per AOI)
#'
#' Like get_seasonal_activation_or() but each AOI uses its own config row.
#' Needed when paired configs have different thresholds across AOIs.
#'
#' @param config_row_1 single-row tibble with threshold columns for AOI 1
#' @param config_row_2 single-row tibble with threshold columns for AOI 2
#' @param trigger_data_1 list from build_trigger_lookup() for AOI 1
#' @param trigger_data_2 list from build_trigger_lookup() for AOI 2
#' @param eval_start start year of evaluation window
#' @param eval_end end year of evaluation window
#' @param aoi1_label character label for AOI 1 (default "AOI1")
#' @param aoi2_label character label for AOI 2 (default "AOI2")
#' @return list with per-AOI primera/postrera trigger years and OR unions
#' @export
get_seasonal_activation_or_paired <- function(config_row_1,
                                               config_row_2,
                                               trigger_data_1,
                                               trigger_data_2,
                                               eval_start, eval_end,
                                               aoi1_label = "AOI1",
                                               aoi2_label = "AOI2") {
  get_trigger_yrs <- function(config_row, tl, lts) {
    yrs <- integer(0)
    for (col in lts) {
      val <- config_row[[col]]
      if (is.na(val)) next
      key <- sprintf("%s_%.4f", col, val)
      y <- tl[[key]]
      if (!is.null(y)) yrs <- union(yrs, y)
    }
    yrs[yrs >= eval_start & yrs <= eval_end]
  }

  all_lts <- c("p0", "p1", "p2", "s0", "s1", "s2", "s3")

  # AOI 1 active LTs (from config_row_1)
  active_1 <- all_lts[all_lts %in% names(config_row_1) &
                         !is.na(unlist(config_row_1[all_lts[all_lts %in% names(config_row_1)]]))]
  p_lts_1 <- active_1[startsWith(active_1, "p")]
  s_lts_1 <- active_1[startsWith(active_1, "s")]

  # AOI 2 active LTs (from config_row_2)
  active_2 <- all_lts[all_lts %in% names(config_row_2) &
                         !is.na(unlist(config_row_2[all_lts[all_lts %in% names(config_row_2)]]))]
  p_lts_2 <- active_2[startsWith(active_2, "p")]
  s_lts_2 <- active_2[startsWith(active_2, "s")]

  tl1 <- trigger_data_1$trigger_lookup
  tl2 <- trigger_data_2$trigger_lookup

  result <- list()
  result[[paste0(aoi1_label, "_primera")]]  <- get_trigger_yrs(config_row_1, tl1, p_lts_1)
  result[[paste0(aoi1_label, "_postrera")]] <- get_trigger_yrs(config_row_1, tl1, s_lts_1)
  result[[paste0(aoi2_label, "_primera")]]  <- get_trigger_yrs(config_row_2, tl2, p_lts_2)
  result[[paste0(aoi2_label, "_postrera")]] <- get_trigger_yrs(config_row_2, tl2, s_lts_2)
  result$primera  <- union(result[[paste0(aoi1_label, "_primera")]],
                            result[[paste0(aoi2_label, "_primera")]])
  result$postrera <- union(result[[paste0(aoi1_label, "_postrera")]],
                            result[[paste0(aoi2_label, "_postrera")]])

  result
}


#' Pair configs from two independent AOI evaluations via OR logic
#'
#' Each AOI has its own pool of evaluated configs (from evaluate_configs_impact).
#' This function finds all valid pairs where the OR-union of trigger years
#' meets must-hit constraints and falls within a target RP range.
#'
#' Strategy for efficiency:
#'   1. Pre-filter each pool to desired LT counts
#'   2. Precompute trigger years for each candidate from trigger_lookup
#'   3. Use must-hit bitmask compatibility to avoid invalid pairs
#'   4. Apply early RP filter before computing F1
#'
#' @param results_1 tibble from evaluate_configs_impact() for AOI 1
#' @param trigger_data_1 trigger_data list for AOI 1
#' @param results_2 tibble from evaluate_configs_impact() for AOI 2
#' @param trigger_data_2 trigger_data list for AOI 2
#' @param impact_years integer vector of EM-DAT drought years
#' @param must_hit_years integer vector of priority 1 years (OR union must cover all)
#' @param should_hit_years integer vector of priority 2 years
#' @param eval_start start year of evaluation window (default 1991)
#' @param eval_end end year of evaluation window (default 2024)
#' @param target_rp_range length-2 numeric: min and max acceptable OR-union annual RP
#' @return tibble with one row per valid pair, including OR-union metrics,
#'   per-AOI config IDs and thresholds, and a same_config flag
#' @export
pair_or_configs <- function(results_1, trigger_data_1,
                            results_2, trigger_data_2,
                            impact_years,
                            must_hit_years = integer(0),
                            should_hit_years = integer(0),
                            eval_start = 1991, eval_end = 2024,
                            target_rp_range = c(3, 5)) {
  eval_window <- eval_start:eval_end
  n_years <- length(eval_window)
  impact_yrs <- sort(intersect(impact_years, eval_window))
  must_hit <- intersect(must_hit_years, eval_window)
  should_hit <- intersect(should_hit_years, eval_window)

  threshold_cols <- intersect(c("p0", "p1", "p2", "s0", "s1", "s2", "s3"),
                              names(results_1))
  p_cols <- threshold_cols[startsWith(threshold_cols, "p")]
  s_cols <- threshold_cols[startsWith(threshold_cols, "s")]

  # ── Helper: extract trigger years from a config row + trigger_lookup ──────
  extract_trigger_yrs <- function(row, tl) {
    p_yrs <- integer(0)
    s_yrs <- integer(0)
    for (col in p_cols) {
      val <- row[[col]]
      if (is.na(val)) next
      key <- sprintf("%s_%.4f", col, val)
      yrs <- tl[[key]] %||% integer(0)
      p_yrs <- union(p_yrs, intersect(yrs, eval_window))
    }
    for (col in s_cols) {
      val <- row[[col]]
      if (is.na(val)) next
      key <- sprintf("%s_%.4f", col, val)
      yrs <- tl[[key]] %||% integer(0)
      s_yrs <- union(s_yrs, intersect(yrs, eval_window))
    }
    list(primera = p_yrs, postrera = s_yrs, all = union(p_yrs, s_yrs))
  }

  # ── Precompute trigger years for all candidates ───────────────────────────
  tl1 <- trigger_data_1$trigger_lookup
  tl2 <- trigger_data_2$trigger_lookup

  cat("Precomputing trigger years for AOI 1 (", nrow(results_1), " configs)...\n")
  triggers_1 <- lapply(seq_len(nrow(results_1)), function(i)
    extract_trigger_yrs(results_1[i, ], tl1))

  cat("Precomputing trigger years for AOI 2 (", nrow(results_2), " configs)...\n")
  triggers_2 <- lapply(seq_len(nrow(results_2)), function(i)
    extract_trigger_yrs(results_2[i, ], tl2))

  # ── Cross-join all pairs with RP filter only ────────────────────────────────
  # must_hit_score and should_hit_score are computed as columns (soft constraints)
  rp_lo <- target_rp_range[1]
  rp_hi <- target_rp_range[2]

  n1 <- nrow(results_1)
  n2 <- nrow(results_2)
  cat(sprintf("Pairing %s x %s = %s candidates (RP filter %.1f-%.1f)...\n",
              format(n1, big.mark = ","), format(n2, big.mark = ","),
              format(n1 * n2, big.mark = ","), rp_lo, rp_hi))

  rows <- vector("list", n1 * n2)
  row_count <- 0L
  total_pairs_checked <- 0L

  for (i1 in seq_len(n1)) {
    t1 <- triggers_1[[i1]]
    for (i2 in seq_len(n2)) {
      total_pairs_checked <- total_pairs_checked + 1L
      t2 <- triggers_2[[i2]]

      # OR-union trigger years
      or_p <- union(t1$primera, t2$primera)
      or_s <- union(t1$postrera, t2$postrera)
      or_all <- union(or_p, or_s)
      n_or <- length(or_all)

      # Early RP filter (only hard constraint)
      or_rp <- if (n_or > 0) (n_years + 1) / n_or else Inf
      if (or_rp < rp_lo || or_rp > rp_hi) next

      n_or_p <- length(or_p)
      n_or_s <- length(or_s)
      or_p_rp <- if (n_or_p > 0) (n_years + 1) / n_or_p else Inf
      or_s_rp <- if (n_or_s > 0) (n_years + 1) / n_or_s else Inf

      # Impact F1 on OR union
      p_tp <- length(intersect(or_p, impact_yrs))
      p_fp <- length(setdiff(or_p, impact_yrs))
      p_fn <- length(setdiff(impact_yrs, or_p))
      s_tp <- length(intersect(or_s, impact_yrs))
      s_fp <- length(setdiff(or_s, impact_yrs))
      s_fn <- length(setdiff(impact_yrs, or_s))

      p_prec <- if (p_tp + p_fp > 0) p_tp / (p_tp + p_fp) else 0
      p_rec  <- if (p_tp + p_fn > 0) p_tp / (p_tp + p_fn) else 0
      p_f1   <- if (p_prec + p_rec > 0) 2 * p_prec * p_rec / (p_prec + p_rec) else 0

      s_prec <- if (s_tp + s_fp > 0) s_tp / (s_tp + s_fp) else 0
      s_rec  <- if (s_tp + s_fn > 0) s_tp / (s_tp + s_fn) else 0
      s_f1   <- if (s_prec + s_rec > 0) 2 * s_prec * s_rec / (s_prec + s_rec) else 0

      # Must-hit and should-hit scores on OR union (soft — just columns)
      mh_score <- if (length(must_hit) > 0) {
        sum(must_hit %in% or_all) / length(must_hit)
      } else 1

      sh_score <- if (length(should_hit) > 0) {
        sum(should_hit %in% or_all) / length(should_hit)
      } else 1

      # Same-config flag
      same_cfg <- all(vapply(threshold_cols, function(col) {
        v1 <- results_1[[col]][i1]
        v2 <- results_2[[col]][i2]
        (is.na(v1) && is.na(v2)) || (!is.na(v1) && !is.na(v2) && v1 == v2)
      }, logical(1)))

      row_count <- row_count + 1L
      rows[[row_count]] <- list(
        aoi1_config_id  = results_1$config_id[i1],
        aoi2_config_id  = results_2$config_id[i2],
        or_annual_rp    = or_rp,
        or_p_seasonal_rp = or_p_rp,
        or_s_seasonal_rp = or_s_rp,
        or_primera_f1   = p_f1,
        or_postrera_f1  = s_f1,
        or_mean_f1      = (p_f1 + s_f1) / 2,
        must_hit_score  = mh_score,
        should_hit_score = sh_score,
        same_config     = same_cfg,
        aoi1_annual_rp  = results_1$annual_rp[i1],
        aoi2_annual_rp  = results_2$annual_rp[i2],
        aoi1_mean_f1    = results_1$mean_f1[i1],
        aoi2_mean_f1    = results_2$mean_f1[i2]
      )
    }
  }

  cat(sprintf("Checked %s pairs, kept %s (RP %.1f-%.1f)\n",
              format(total_pairs_checked, big.mark = ","),
              format(row_count, big.mark = ","),
              rp_lo, rp_hi))

  result <- dplyr::bind_rows(rows[seq_len(row_count)])

  if (nrow(result) == 0) {
    warning("No valid pairs found. Try widening target_rp_range or relaxing must-hit.")
    return(result)
  }

  # ── Attach per-AOI threshold columns ────────────────────────────────────
  aoi1_thresholds <- results_1 |>
    dplyr::select(aoi1_config_id = config_id, all_of(threshold_cols)) |>
    dplyr::rename_with(~ paste0("aoi1_", .), all_of(threshold_cols))

  aoi2_thresholds <- results_2 |>
    dplyr::select(aoi2_config_id = config_id, all_of(threshold_cols)) |>
    dplyr::rename_with(~ paste0("aoi2_", .), all_of(threshold_cols))

  result <- result |>
    dplyr::left_join(aoi1_thresholds, by = "aoi1_config_id") |>
    dplyr::left_join(aoi2_thresholds, by = "aoi2_config_id")

  result
}
