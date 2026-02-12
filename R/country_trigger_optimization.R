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

    # Seasonal and annual RPs
    p_rp <- if (n_p > 0) n_years / n_p else Inf
    s_rp <- if (n_s > 0) n_years / n_s else Inf
    annual_triggers <- union(p_trigger_yrs, s_trigger_yrs)
    annual_rp <- if (length(annual_triggers) > 0) n_years / length(annual_triggers) else Inf

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
      primera_tol_f1 = p_tol_f1, postrera_tol_f1 = s_tol_f1,
      mean_tol_f1 = (p_tol_f1 + s_tol_f1) / 2,
      !!!ew_vals,
      mean_f1 = (p_f1 + s_f1) / 2,
      f1_diff = abs(p_f1 - s_f1),
      rp_diff = s_rp - p_rp,
      p_cv = p_cv,
      s_cv = s_cv,
      mean_cv = (p_cv + s_cv) / 2
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
#' 1. Filter to target LT set and closest annual RP(s)
#' 2. Keep configs with best mean F1 (at each RP)
#' 3. Pick archetype closest to target RP with highest F1
#' 4. Within that archetype, prefer simplest config:
#'    flat thresholds (CV=0) > lowest CV > fewest marginal FPs
#'
#' Returns the single config row with a text rationale.
#'
#' @param df tibble from evaluate_configs() (must have add_lt_set() applied)
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
