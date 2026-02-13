box::use(
  dplyr[...],
  tidyr[expand_grid, replace_na, pivot_longer, pivot_wider],
  purrr[map_dfr],
  stats[setNames, na.omit, sd],
  utils[head, tail],
  ggplot2[...],
  ggrepel[geom_label_repel, geom_text_repel],
  patchwork[...],
  ggnewscale[new_scale_fill],
  gt[...],
  pROC[roc, auc, coords, ci.auc]
)

# ── Internal color palettes ──────────────────────────────────────────────────

config_detail_colors <- c(
  "TP (first caught)"   = "#2ca02c",
  "TP (already caught)" = "#98df8a",
  "FP (marginal)"       = "#d62728",
  "FP (redundant)"      = "#ffbb78",
  "FN (missed)"         = "#7f7f7f",
  "TN"                  = "white",
  "TP"                  = "#2ca02c",
  "FP"                  = "#d62728",
  "FN"                  = "#7f7f7f",
  "Drought (RP 10+)"    = "#a50f15",
  "Drought (RP 5-10)"   = "#de2d26",
  "Drought (RP <5)"     = "#fee0d2"
)

timing_colors <- c(
  "TP (LT0)" = "#C6EDE9",
  "TP (LT1)" = "#78D9D1",
  "TP (LT2)" = "#3BB8AD",
  "TP (LT3)" = "#1F8A81",
  "FP"       = "#F7A29C",
  "FN"       = "#d0d0d0",
  "TN"       = "#E0EBF5"
)
timing_highlight <- "#0063B3"

# ── Exported helpers ─────────────────────────────────────────────────────────

#' Generate leadtime display labels from issued months
#'
#' Maps LT IDs (p0, p1, ..., s0, s1, ...) to display labels like "LT0 (May)".
#' LT0 = latest issued month (shortest lead), higher LTs = earlier issued.
#'
#' @param primera_issued_months integer vector of primera issued months
#' @param postrera_issued_months integer vector of postrera issued months
#' @return named character vector
#' @export
make_lt_labels <- function(primera_issued_months, postrera_issued_months) {
  month_abbr <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  p_sorted <- sort(primera_issued_months, decreasing = TRUE)
  s_sorted <- sort(postrera_issued_months, decreasing = TRUE)
  c(
    setNames(
      sprintf("LT%d (%s)", seq_along(p_sorted) - 1, month_abbr[p_sorted]),
      paste0("p", seq_along(p_sorted) - 1)
    ),
    setNames(
      sprintf("LT%d (%s)", seq_along(s_sorted) - 1, month_abbr[s_sorted]),
      paste0("s", seq_along(s_sorted) - 1)
    )
  )
}


# ── Plot functions ───────────────────────────────────────────────────────────

#' Heatmap showing per-LT trigger classification for a single config
#'
#' Replicates the ch10 fig-lt-value heatmap for any config. Each active LT
#' gets a row showing TP (first caught / already caught), FP (marginal /
#' redundant), FN, and TN per year. Includes season trigger summary and
#' observed drought severity rows.
#'
#' @param config_row single-row tibble with threshold columns + metrics
#' @param trigger_data list from build_trigger_lookup()
#' @param df_joined data.frame with year, window, leadtime, fcst_mm, obs_mm
#' @param lt_labels named character vector from make_lt_labels() (optional)
#' @return patchwork ggplot object
#' @export
plot_config_detail <- function(config_row, trigger_data, df_joined, lt_labels = NULL) {
  if (is.null(lt_labels)) {
    lt_labels <- c(
      p0 = "LT0 (May)", p1 = "LT1 (Apr)", p2 = "LT2 (Mar)",
      s0 = "LT0 (Sep)", s1 = "LT1 (Aug)", s2 = "LT2 (Jul)", s3 = "LT3 (Jun)"
    )
  }
  all_colors <- config_detail_colors

  tl <- trigger_data$trigger_lookup
  primera_ranked <- trigger_data$primera_ranked
  postrera_ranked <- trigger_data$postrera_ranked
  n_years <- trigger_data$n_years
  all_years <- sort(unique(df_joined$year))

  p_cols <- intersect(c("p0", "p1", "p2"), names(config_row))
  s_cols <- intersect(c("s0", "s1", "s2", "s3"), names(config_row))

  get_yrs <- function(id, val) {
    if (is.na(val)) return(NULL)
    key <- sprintf("%s_%.4f", id, val)
    tl[[key]] %||% integer(0)
  }

  p_lt_yrs <- list()
  for (col in p_cols) {
    yrs <- get_yrs(col, config_row[[col]])
    if (!is.null(yrs)) p_lt_yrs[[col]] <- yrs
  }
  s_lt_yrs <- list()
  for (col in s_cols) {
    yrs <- get_yrs(col, config_row[[col]])
    if (!is.null(yrs)) s_lt_yrs[[col]] <- yrs
  }

  p_union <- Reduce(union, p_lt_yrs)
  s_union <- Reduce(union, s_lt_yrs)
  p_drought <- head(primera_ranked, length(p_union))
  s_drought <- head(postrera_ranked, length(s_union))

  # Classify each year x LT
  classify_lt <- function(lt_yrs, later_lt_yrs, drought_yrs, lt_label, season_label) {
    map_dfr(all_years, function(yr) {
      triggered <- yr %in% lt_yrs
      is_drought <- yr %in% drought_yrs
      in_later <- yr %in% later_lt_yrs
      status <- case_when(
        triggered & is_drought & !in_later ~ "TP (first caught)",
        triggered & is_drought & in_later  ~ "TP (already caught)",
        triggered & !is_drought & !in_later ~ "FP (marginal)",
        triggered & !is_drought & in_later  ~ "FP (redundant)",
        !triggered & is_drought             ~ "FN (missed)",
        TRUE                                ~ "TN"
      )
      tibble(year = yr, row = lt_label, season = season_label, status = status)
    })
  }

  lt_data_list <- list()
  p_ids <- rev(names(p_lt_yrs))
  p_cumulative <- integer(0)
  for (id in p_ids) {
    lt_data_list <- c(lt_data_list, list(
      classify_lt(p_lt_yrs[[id]], p_cumulative, p_drought, lt_labels[[id]], "Primera")
    ))
    p_cumulative <- union(p_cumulative, p_lt_yrs[[id]])
  }

  s_ids <- rev(names(s_lt_yrs))
  s_cumulative <- integer(0)
  for (id in s_ids) {
    lt_data_list <- c(lt_data_list, list(
      classify_lt(s_lt_yrs[[id]], s_cumulative, s_drought, lt_labels[[id]], "Postrera")
    ))
    s_cumulative <- union(s_cumulative, s_lt_yrs[[id]])
  }

  lt_data <- bind_rows(lt_data_list)

  # Forecast RP annotations per year per LT
  fcst_rp <- df_joined |>
    group_by(window, leadtime) |>
    mutate(
      dry_rank = rank(fcst_mm, ties.method = "first"),
      fcst_rp  = (n() + 1) / dry_rank
    ) |>
    ungroup() |>
    mutate(
      lt_id = paste0(if_else(window == "primera", "p", "s"), leadtime),
      row = lt_labels[lt_id],
      season = if_else(window == "primera", "Primera", "Postrera"),
      rp_label = sprintf("%.1f", fcst_rp)
    ) |>
    filter(!is.na(row)) |>
    select(year, row, season, rp_label)

  active_labels <- lt_labels[c(names(p_lt_yrs), names(s_lt_yrs))]
  fcst_rp <- fcst_rp |> filter(row %in% active_labels)

  lt_data <- lt_data |>
    left_join(fcst_rp, by = c("year", "row", "season"))

  # Season trigger summary row
  season_trigger <- bind_rows(
    tibble(year = all_years, season = "Primera",
           triggered = year %in% p_union, drought = year %in% p_drought),
    tibble(year = all_years, season = "Postrera",
           triggered = year %in% s_union, drought = year %in% s_drought)
  ) |>
    mutate(
      row = "Season trigger",
      status = case_when(
        triggered & drought  ~ "TP",
        triggered & !drought ~ "FP",
        !triggered & drought ~ "FN",
        TRUE                 ~ "TN"
      )
    ) |>
    select(year, row, season, status)

  # Observed drought severity row
  obs_by_year <- df_joined |> distinct(year, window, obs_mm)
  obs_rp_data <- obs_by_year |>
    group_by(window) |>
    mutate(
      dry_rank = rank(obs_mm, ties.method = "first"),
      obs_rp = (n() + 1) / dry_rank
    ) |>
    ungroup()

  obs_row <- obs_rp_data |>
    mutate(
      season = if_else(window == "primera", "Primera", "Postrera"),
      row = "Observed (ERA5)",
      is_drought = (year %in% p_drought & season == "Primera") |
                   (year %in% s_drought & season == "Postrera"),
      status = case_when(
        !is_drought  ~ "TN",
        obs_rp >= 10 ~ "Drought (RP 10+)",
        obs_rp >= 5  ~ "Drought (RP 5-10)",
        TRUE         ~ "Drought (RP <5)"
      ),
      rp_label = sprintf("%.1f", obs_rp)
    ) |>
    select(year, row, season, status, rp_label)

  # Build facet plots
  build_season_plot <- function(season_name, row_levels, show_x = FALSE) {
    d <- bind_rows(
      lt_data |> filter(season == season_name),
      season_trigger |> filter(season == season_name),
      obs_row |> filter(season == season_name)
    ) |>
      mutate(
        year   = factor(year, levels = all_years),
        row    = factor(row, levels = rev(row_levels)),
        status = factor(status, levels = names(all_colors))
      )

    ggplot(d, aes(x = year, y = row, fill = status)) +
      geom_tile(color = "black", linewidth = 0.4) +
      geom_text(
        data = d |> filter(!is.na(rp_label)),
        aes(label = rp_label), size = 2
      ) +
      scale_fill_manual(values = all_colors, name = NULL, drop = FALSE) +
      scale_x_discrete(drop = FALSE) +
      labs(x = NULL, y = NULL, subtitle = season_name) +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = if (show_x) element_text(angle = 45, hjust = 1, size = 8)
                       else element_blank(),
        panel.grid    = element_blank(),
        legend.position = "none",
        plot.subtitle = element_text(face = "bold", size = 13)
      )
  }

  p_row_levels <- c(
    rev(lt_labels[names(p_lt_yrs)]),
    "Season trigger", "Observed (ERA5)"
  )
  s_row_levels <- c(
    rev(lt_labels[names(s_lt_yrs)]),
    "Season trigger", "Observed (ERA5)"
  )

  p_plot <- build_season_plot("Primera", p_row_levels, show_x = FALSE)
  s_plot <- build_season_plot("Postrera", s_row_levels, show_x = TRUE)

  fmt_rp <- function(x) if (is.na(x)) "\u2014" else sprintf("%.1f", x)
  p_str <- paste(paste0(p_cols, "=", sapply(config_row[p_cols], fmt_rp)), collapse = "  ")
  s_str <- paste(paste0(s_cols, "=", sapply(config_row[s_cols], fmt_rp)), collapse = "  ")

  (p_plot / s_plot) +
    plot_layout(heights = c(length(p_row_levels), length(s_row_levels))) +
    plot_annotation(
      title = sprintf("Trigger config: %s  |  %s", p_str, s_str),
      subtitle = sprintf(
        "Annual RP = %.2f | Matched F1: primera = %.0f%%, postrera = %.0f%%, mean = %.0f%%",
        config_row$annual_rp,
        config_row$primera_f1 * 100, config_row$postrera_f1 * 100,
        config_row$mean_f1 * 100
      ),
      theme = theme(
        plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11)
      )
    )
}


# ── Comparison tables ────────────────────────────────────────────────────────

#' Summary comparison table for multiple configs
#'
#' GT table with sections: Performance, Early Warning, Trigger Year Differences.
#'
#' @param config_rows tibble of candidate configs (from evaluate_configs)
#' @param trigger_data list from build_trigger_lookup()
#' @return gt table object
#' @export
compare_configs <- function(config_rows, trigger_data) {
  tl <- trigger_data$trigger_lookup
  n_configs <- nrow(config_rows)

  p_cols <- intersect(c("p0", "p1", "p2"), names(config_rows))
  s_cols <- intersect(c("s0", "s1", "s2", "s3"), names(config_rows))
  fmt_rp <- function(x) if (is.na(x)) "\u2014" else sprintf("%.1f", x)

  config_labels <- vapply(seq_len(n_configs), function(i) {
    r <- config_rows[i, ]
    p_str <- paste(sapply(r[p_cols], fmt_rp), collapse = "/")
    s_str <- paste(sapply(r[s_cols], fmt_rp), collapse = "/")
    sprintf("Config %d\n%s | %s", i, p_str, s_str)
  }, character(1))

  # Trigger years per config per season
  get_trigger_years <- function(row, cols) {
    yrs_list <- lapply(cols, function(col) {
      val <- row[[col]]
      if (is.na(val)) return(integer(0))
      key <- sprintf("%s_%.4f", col, val)
      tl[[key]] %||% integer(0)
    })
    Reduce(union, yrs_list)
  }

  p_triggers <- lapply(seq_len(n_configs), function(i)
    get_trigger_years(config_rows[i, ], p_cols))
  s_triggers <- lapply(seq_len(n_configs), function(i)
    get_trigger_years(config_rows[i, ], s_cols))

  # Unique trigger years per config
  p_unique <- lapply(seq_len(n_configs), function(i)
    setdiff(p_triggers[[i]], Reduce(union, p_triggers[-i])))
  s_unique <- lapply(seq_len(n_configs), function(i)
    setdiff(s_triggers[[i]], Reduce(union, s_triggers[-i])))

  fmt_years <- function(yrs) {
    if (length(yrs) == 0) return("\u2014")
    paste(sort(yrs), collapse = ", ")
  }

  # Performance section
  metrics <- tibble(
    section = "Performance",
    metric = c("Annual RP", "Primera RP", "Postrera RP",
               "Mean F1", "Primera F1", "Postrera F1",
               "Mean CV")
  )
  for (i in seq_len(n_configs)) {
    r <- config_rows[i, ]
    metrics[[config_labels[i]]] <- c(
      sprintf("%.2f", r$annual_rp),
      sprintf("%.1f", r$p_seasonal_rp),
      sprintf("%.1f", r$s_seasonal_rp),
      sprintf("%.0f%%", r$mean_f1 * 100),
      sprintf("%.0f%%", r$primera_f1 * 100),
      sprintf("%.0f%%", r$postrera_f1 * 100),
      sprintf("%.3f", r$mean_cv)
    )
  }

  # Early warning section
  ew <- tibble(
    section = "Early Warning",
    metric = c("Primera early TPs", "Postrera early TPs",
               "Primera marginal FPs", "Postrera marginal FPs",
               "Early:MFP ratio")
  )
  for (i in seq_len(n_configs)) {
    r <- config_rows[i, ]
    ew[[config_labels[i]]] <- c(
      as.character(r$p_early_tp),
      as.character(r$s_early_tp),
      as.character(r$p_marginal_fp),
      as.character(r$s_marginal_fp),
      sprintf("%.1f", r$early_mfp_ratio)
    )
  }

  # Trigger year diffs (always shown)
  trig_p <- tibble(section = "Trigger Year Differences",
                   metric = "Primera (unique to config)")
  trig_s <- tibble(section = "Trigger Year Differences",
                   metric = "Postrera (unique to config)")
  for (i in seq_len(n_configs)) {
    trig_p[[config_labels[i]]] <- fmt_years(p_unique[[i]])
    trig_s[[config_labels[i]]] <- fmt_years(s_unique[[i]])
  }
  trig <- bind_rows(trig_p, trig_s)

  comparison <- bind_rows(metrics, ew, trig)

  comparison |>
    gt(groupname_col = "section") |>
    cols_label(metric = "") |>
    tab_header(title = sprintf("Config Comparison (%d candidates)", n_configs)) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) |>
    tab_style(style = cell_text(size = px(11)), locations = cells_body()) |>
    cols_align(align = "center", columns = -metric) |>
    tab_options(column_labels.font.size = px(10), data_row.padding = px(3))
}


#' Timing diff table showing years where configs disagree
#'
#' GT table filtered to only years where configs have different trigger
#' timing or status. Shared behavior is summarized in a footnote.
#'
#' @param config_rows tibble of candidate configs
#' @param trigger_data list from build_trigger_lookup()
#' @return gt table object
#' @export
compare_configs_timing <- function(config_rows, trigger_data) {
  tl <- trigger_data$trigger_lookup
  primera_ranked <- trigger_data$primera_ranked
  postrera_ranked <- trigger_data$postrera_ranked
  n_configs <- nrow(config_rows)

  p_cols <- intersect(c("p0", "p1", "p2"), names(config_rows))
  s_cols <- intersect(c("s0", "s1", "s2", "s3"), names(config_rows))
  fmt_rp <- function(x) if (is.na(x)) "\u2014" else sprintf("%.1f", x)

  config_labels <- vapply(seq_len(n_configs), function(i) {
    r <- config_rows[i, ]
    p_str <- paste(sapply(r[p_cols], fmt_rp), collapse = "/")
    s_str <- paste(sapply(r[s_cols], fmt_rp), collapse = "/")
    sprintf("Config %d\n%s | %s", i, p_str, s_str)
  }, character(1))

  get_lt_yrs <- function(row, cols) {
    out <- list()
    for (col in cols) {
      val <- row[[col]]
      if (!is.na(val)) {
        key <- sprintf("%s_%.4f", col, val)
        out[[col]] <- tl[[key]] %||% integer(0)
      }
    }
    out
  }

  earliest_lt_per_year <- function(row, cols, drought_yrs) {
    lt_yrs <- get_lt_yrs(row, cols)
    if (length(lt_yrs) == 0) return(tibble(year = integer(), earliest_lt = character(),
                                            is_tp = logical()))
    lt_ids <- rev(names(lt_yrs))
    assigned <- integer(0)
    results <- list()
    for (lt_id in lt_ids) {
      new_yrs <- setdiff(lt_yrs[[lt_id]], assigned)
      if (length(new_yrs) > 0) {
        results <- c(results, list(tibble(
          year = new_yrs, earliest_lt = lt_id,
          is_tp = new_yrs %in% drought_yrs
        )))
      }
      assigned <- union(assigned, lt_yrs[[lt_id]])
    }
    bind_rows(results)
  }

  build_season_comparison <- function(cols, ranked, season_name) {
    config_data <- lapply(seq_len(n_configs), function(i) {
      row <- config_rows[i, ]
      lt_yrs <- get_lt_yrs(row, cols)
      union_yrs <- if (length(lt_yrs) > 0) Reduce(union, lt_yrs) else integer(0)
      drought <- head(ranked, length(union_yrs))
      df <- earliest_lt_per_year(row, cols, drought)
      df$config <- config_labels[i]
      df$drought <- drought
      list(df = df, drought = drought, union_yrs = union_yrs)
    })

    all_trigger_yrs <- sort(unique(unlist(lapply(config_data, function(x) x$union_yrs))))
    all_drought <- sort(unique(unlist(lapply(config_data, function(x) x$drought))))
    all_relevant_yrs <- sort(unique(c(all_trigger_yrs, all_drought)))
    if (length(all_relevant_yrs) == 0) return(NULL)

    result <- tibble(year = all_relevant_yrs)
    obs_rank <- match(all_relevant_yrs, ranked)
    result$observed <- case_when(
      is.na(obs_rank) ~ "\u2014",
      TRUE            ~ sprintf("Rank %d", obs_rank)
    )
    result$obs_rank <- obs_rank

    for (i in seq_len(n_configs)) {
      df_i <- config_data[[i]]$df
      drought_i <- config_data[[i]]$drought
      col_lt <- paste0("cfg", i, "_lt")
      col_status <- paste0("cfg", i, "_status")

      result[[col_lt]] <- vapply(all_relevant_yrs, function(yr) {
        row <- df_i |> filter(year == yr)
        if (nrow(row) == 0) return("\u2014")
        toupper(row$earliest_lt[1])
      }, character(1))

      result[[col_status]] <- vapply(all_relevant_yrs, function(yr) {
        row <- df_i |> filter(year == yr)
        is_drought <- yr %in% drought_i
        if (nrow(row) == 0 && is_drought) return("FN")
        if (nrow(row) == 0) return("")
        if (row$is_tp[1]) return("TP")
        return("FP")
      }, character(1))
    }

    result$season <- season_name
    result
  }

  p_comp <- build_season_comparison(p_cols, primera_ranked, "Primera")
  s_comp <- build_season_comparison(s_cols, postrera_ranked, "Postrera")
  both <- bind_rows(p_comp, s_comp)

  # Combine LT + status into one cell per config
  display <- both |> select(season, year, obs_rank)
  for (i in seq_len(n_configs)) {
    col_lt <- paste0("cfg", i, "_lt")
    col_status <- paste0("cfg", i, "_status")
    display[[config_labels[i]]] <- case_when(
      both[[col_status]] == "TP" ~ paste0(both[[col_lt]], " \u2713"),
      both[[col_status]] == "FP" ~ paste0(both[[col_lt]], " \u2717"),
      both[[col_status]] == "FN" ~ "missed",
      TRUE ~ ""
    )
  }

  display <- display |>
    mutate(obs_rank = if_else(is.na(obs_rank), "\u2014", as.character(obs_rank))) |>
    rename(`Obs Rank` = obs_rank)

  config_col_names <- config_labels
  differs <- apply(display[config_col_names], 1, function(row) length(unique(row)) > 1)
  display_diff <- display |> filter(differs)

  display_shared <- display |>
    filter(!differs) |>
    group_by(season) |>
    summarise(
      n_shared = n(),
      shared_tp = sum(grepl("\u2713", .data[[config_col_names[1]]])),
      shared_fp = sum(grepl("\u2717", .data[[config_col_names[1]]])),
      shared_fn = sum(.data[[config_col_names[1]]] == "missed"),
      .groups = "drop"
    )

  tbl <- display_diff |>
    gt(groupname_col = "season") |>
    cols_label(year = "Year") |>
    tab_header(
      title = "Config Comparison: Where They Differ",
      subtitle = "Only years where configs disagree | \u2713 = TP, \u2717 = FP"
    ) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups())

  for (col_name in config_col_names) {
    tbl <- tbl |>
      tab_style(
        style = cell_fill(color = "#d4edda"),
        locations = cells_body(columns = all_of(col_name),
                               rows = grepl("\u2713", display_diff[[col_name]]))
      ) |>
      tab_style(
        style = cell_fill(color = "#f8d7da"),
        locations = cells_body(columns = all_of(col_name),
                               rows = grepl("\u2717", display_diff[[col_name]]))
      ) |>
      tab_style(
        style = cell_fill(color = "#e2e3e5"),
        locations = cells_body(columns = all_of(col_name),
                               rows = display_diff[[col_name]] == "missed")
      )
  }

  shared_note <- display_shared |>
    mutate(label = sprintf("%s: %d shared years (%d TP, %d FP, %d FN)",
                           season, n_shared, shared_tp, shared_fp, shared_fn)) |>
    pull(label) |>
    paste(collapse = " | ")

  tbl |>
    tab_options(table.font.size = px(11), data_row.padding = px(2)) |>
    tab_source_note(sprintf("Identical across all configs: %s", shared_note)) |>
    tab_source_note("LT shown is earliest to trigger that year")
}


#' Timing comparison heatmap (ggplot version)
#'
#' Heatmap with configs as rows, years as columns, filled by TP/FP/FN status.
#' Text labels show earliest LT. Blue borders highlight years where configs
#' differ. Faceted by season.
#'
#' @param config_rows tibble of candidate configs
#' @param trigger_data list from build_trigger_lookup()
#' @param df_joined data.frame with year, window, leadtime, fcst_mm, obs_mm
#' @return ggplot object
#' @export
compare_configs_timing2 <- function(config_rows, trigger_data, df_joined) {
  tl <- trigger_data$trigger_lookup
  primera_ranked <- trigger_data$primera_ranked
  postrera_ranked <- trigger_data$postrera_ranked
  n_configs <- nrow(config_rows)
  all_years <- sort(unique(df_joined$year))

  p_cols <- intersect(c("p0", "p1", "p2"), names(config_rows))
  s_cols <- intersect(c("s0", "s1", "s2", "s3"), names(config_rows))
  fmt_rp <- function(x) if (is.na(x)) "\u2014" else sprintf("%.1f", x)

  config_labels <- vapply(seq_len(n_configs), function(i) {
    r <- config_rows[i, ]
    p_str <- paste(sapply(r[p_cols], fmt_rp), collapse = "/")
    s_str <- paste(sapply(r[s_cols], fmt_rp), collapse = "/")
    # Use option for display name if available
    name <- if ("option" %in% names(r) && !is.na(r$option)) {
      r$option
    } else {
      sprintf("Config %d", i)
    }
    sprintf("%s\n%s | %s", name, p_str, s_str)
  }, character(1))

  get_lt_yrs <- function(row, cols) {
    out <- list()
    for (col in cols) {
      val <- row[[col]]
      if (!is.na(val)) {
        key <- sprintf("%s_%.4f", col, val)
        out[[col]] <- tl[[key]] %||% integer(0)
      }
    }
    out
  }

  lt_display <- c(p0 = "LT0", p1 = "LT1", p2 = "LT2",
                  s0 = "LT0", s1 = "LT1", s2 = "LT2", s3 = "LT3")

  build_season_data <- function(cols, ranked, season_name) {
    rows <- list()
    for (i in seq_len(n_configs)) {
      lt_yrs <- get_lt_yrs(config_rows[i, ], cols)
      if (length(lt_yrs) == 0) next
      union_yrs <- Reduce(union, lt_yrs)
      drought <- head(ranked, length(union_yrs))

      lt_ids <- rev(names(lt_yrs))
      assigned <- integer(0)
      for (lt_id in lt_ids) {
        new_yrs <- setdiff(lt_yrs[[lt_id]], assigned)
        if (length(new_yrs) > 0) {
          rows <- c(rows, list(tibble(
            year = new_yrs,
            config = config_labels[i],
            season = season_name,
            earliest_lt = lt_display[[lt_id]],
            status = if_else(new_yrs %in% drought, "TP", "FP")
          )))
        }
        assigned <- union(assigned, lt_yrs[[lt_id]])
      }

      fn_yrs <- setdiff(drought, union_yrs)
      if (length(fn_yrs) > 0) {
        rows <- c(rows, list(tibble(
          year = fn_yrs, config = config_labels[i],
          season = season_name, earliest_lt = NA_character_, status = "FN"
        )))
      }
    }
    bind_rows(rows)
  }

  d <- bind_rows(
    build_season_data(p_cols, primera_ranked, "Primera"),
    build_season_data(s_cols, postrera_ranked, "Postrera")
  )

  # Pad with TN
  full_grid <- expand_grid(
    year = all_years,
    config = config_labels,
    season = c("Primera", "Postrera")
  )
  d <- full_grid |>
    left_join(d, by = c("year", "config", "season")) |>
    mutate(
      status = replace_na(status, "TN"),
      earliest_lt = replace_na(earliest_lt, ""),
      fill_status = case_when(
        status == "TP" ~ paste0("TP (", earliest_lt, ")"),
        TRUE ~ status
      )
    )

  # Flag years where configs differ
  diff_flag <- d |>
    group_by(year, season) |>
    summarise(
      differs = length(unique(paste(status, earliest_lt))) > 1,
      .groups = "drop"
    )
  d <- d |> left_join(diff_flag, by = c("year", "season"))

  d <- d |>
    mutate(
      fill_status = factor(fill_status, levels = names(timing_colors)),
      season = factor(season, levels = c("Primera", "Postrera"))
    )

  ggplot(d, aes(x = factor(year), y = config, fill = fill_status)) +
    geom_tile(color = "grey60", linewidth = 0.3) +
    geom_tile(
      data = d |> filter(differs),
      aes(x = factor(year), y = config),
      fill = NA, color = timing_highlight, linewidth = 1.4
    ) +
    geom_text(
      data = d |> filter(earliest_lt != ""),
      aes(label = earliest_lt), size = 2.5, fontface = "bold"
    ) +
    scale_fill_manual(values = timing_colors, name = "Status", drop = TRUE) +
    facet_wrap(~season, ncol = 1, scales = "free_y") +
    labs(
      x = NULL, y = NULL,
      title = "Config Timing Comparison",
      subtitle = "Text = earliest LT to trigger | Darker green = earlier LT | Blue border = configs differ"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y = element_text(size = 8),
      panel.grid = element_blank(),
      strip.text = element_text(face = "bold", size = 12),
      legend.position = "bottom"
    )
}


#' Cross-archetype year comparison heatmap
#'
#' Takes one representative config per archetype and shows season-level
#' TP/FP/FN/TN classification per year. No LT detail (since LT timing
#' varies within archetypes). Blue border highlights years that are
#' classified differently across archetypes.
#'
#' @param representatives tibble from summarise_archetypes()$representatives
#' @param trigger_data list from build_trigger_lookup()
#' @param df_joined tibble with year, window, leadtime, fcst_mm, obs_mm
#' @return ggplot object
#' @export
compare_archetypes_timing <- function(representatives, trigger_data, df_joined) {
  tl <- trigger_data$trigger_lookup
  primera_ranked <- trigger_data$primera_ranked
  postrera_ranked <- trigger_data$postrera_ranked
  all_years <- sort(unique(df_joined$year))
  n_arch <- nrow(representatives)

  p_cols <- intersect(c("p0", "p1", "p2"), names(representatives))
  s_cols <- intersect(c("s0", "s1", "s2", "s3"), names(representatives))

  # Labels: archetype name only (no thresholds)
  arch_labels <- vapply(seq_len(n_arch), function(i) {
    r <- representatives[i, ]
    if ("arch_label" %in% names(r) && !is.na(r$arch_label[[1]])) {
      as.character(r$arch_label[[1]])
    } else if ("option" %in% names(r) && !is.na(r$option[[1]])) {
      as.character(r$option[[1]])
    } else {
      sprintf("Archetype %d", i)
    }
  }, character(1))

  # Build season-level classification (no LT breakdown)
  get_union_yrs <- function(row, cols) {
    yrs <- list()
    for (col in cols) {
      val <- row[[col]]
      if (!is.na(val)) {
        key <- sprintf("%s_%.4f", col, val)
        yrs[[col]] <- tl[[key]] %||% integer(0)
      }
    }
    if (length(yrs) == 0) return(integer(0))
    Reduce(union, yrs)
  }

  build_season <- function(cols, ranked, season_name) {
    rows <- list()
    for (i in seq_len(n_arch)) {
      trigger_yrs <- get_union_yrs(representatives[i, ], cols)
      drought <- head(ranked, length(trigger_yrs))

      for (yr in all_years) {
        triggered <- yr %in% trigger_yrs
        is_drought <- yr %in% drought
        status <- if (triggered && is_drought) "TP"
                  else if (triggered && !is_drought) "FP"
                  else if (!triggered && is_drought) "FN"
                  else "TN"
        rows <- c(rows, list(tibble(
          year = yr, archetype = arch_labels[i],
          season = season_name, status = status
        )))
      }
    }
    bind_rows(rows)
  }

  d <- bind_rows(
    build_season(p_cols, primera_ranked, "Primera"),
    build_season(s_cols, postrera_ranked, "Postrera")
  )

  # Flag years where archetypes differ
  diff_flag <- d |>
    group_by(year, season) |>
    summarise(differs = length(unique(status)) > 1, .groups = "drop")
  d <- d |> left_join(diff_flag, by = c("year", "season"))

  # Build ERA5 rank row per season
  n_yrs <- length(all_years)
  era5_rank_label <- "ERA5 rank"
  p_rank <- setNames(seq_along(primera_ranked), primera_ranked)
  s_rank <- setNames(seq_along(postrera_ranked), postrera_ranked)

  d_era5 <- bind_rows(
    tibble(
      year = all_years, archetype = era5_rank_label, season = "Primera",
      status = NA_character_, differs = FALSE,
      rank = as.integer(p_rank[as.character(all_years)])
    ),
    tibble(
      year = all_years, archetype = era5_rank_label, season = "Postrera",
      status = NA_character_, differs = FALSE,
      rank = as.integer(s_rank[as.character(all_years)])
    )
  )

  # Archetype data gets rank = NA
  arch_colors <- c("TP" = "#2ca02c", "FP" = "#d62728",
                    "FN" = "#d0d0d0", "TN" = "#E0EBF5")
  d <- d |> mutate(rank = NA_integer_)
  d_all <- bind_rows(d, d_era5)

  y_levels <- c(era5_rank_label, arch_labels)
  d_all <- d_all |>
    mutate(
      status = factor(status, levels = names(arch_colors)),
      season = factor(season, levels = c("Primera", "Postrera")),
      archetype = factor(archetype, levels = y_levels)
    )

  d_era5_plot <- d_all |> filter(archetype == era5_rank_label)
  d_arch_plot <- d_all |> filter(archetype != era5_rank_label)

  ggplot() +
    # Archetype tiles with status fill
    geom_tile(
      data = d_arch_plot,
      aes(x = factor(year), y = archetype, fill = status),
      color = "grey60", linewidth = 0.3
    ) +
    scale_fill_manual(values = arch_colors, name = "Status", drop = TRUE) +
    # Diff highlight
    geom_tile(
      data = d_arch_plot |> filter(differs),
      aes(x = factor(year), y = archetype),
      fill = NA, color = timing_highlight, linewidth = 1.4
    ) +
    # New fill scale for ERA5 rank
    new_scale_fill() +
    geom_tile(
      data = d_era5_plot,
      aes(x = factor(year), y = archetype, fill = rank),
      color = "grey60", linewidth = 0.3
    ) +
    geom_text(
      data = d_era5_plot,
      aes(x = factor(year), y = archetype, label = rank),
      size = 2.2, fontface = "bold",
      color = ifelse(d_era5_plot$rank <= n_yrs * 0.4, "white", "grey30")
    ) +
    scale_fill_gradient(
      low = "#8B4513", high = "#FDEBD0",
      name = "ERA5\nRank"
    ) +
    facet_wrap(~season, ncol = 1, scales = "free_y") +
    labs(
      x = NULL, y = NULL,
      title = "Archetype Year Comparison",
      subtitle = "Blue border = archetypes differ | Bottom row = ERA5 drought rank (1 = driest)"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y = element_text(size = 9, face = "bold"),
      panel.grid = element_blank(),
      strip.text = element_text(face = "bold", size = 12),
      legend.position = "bottom"
    )
}


#' Per-leadtime value comparison table
#'
#' GT table with configs as row groups, seasons as rows, and LTs as columns.
#' Each cell shows marginal TP/FP contribution. Color-coded: green = pure
#' value (+TP/+0FP), red = pure cost (+0TP/+FP), yellow = mixed.
#'
#' @param config_rows tibble of candidate configs
#' @param trigger_data list from build_trigger_lookup()
#' @return gt table object
#' @export
compare_configs_lt_value <- function(config_rows, trigger_data) {
  n_configs <- nrow(config_rows)
  p_cols <- intersect(c("p0", "p1", "p2"), names(config_rows))
  s_cols <- intersect(c("s0", "s1", "s2", "s3"), names(config_rows))
  fmt_rp <- function(x) if (is.na(x)) "\u2014" else sprintf("%.1f", x)

  config_labels <- vapply(seq_len(n_configs), function(i) {
    r <- config_rows[i, ]
    p_str <- paste(sapply(r[p_cols], fmt_rp), collapse = "/")
    s_str <- paste(sapply(r[s_cols], fmt_rp), collapse = "/")
    sprintf("Config %d (%s | %s)", i, p_str, s_str)
  }, character(1))

  p_lts <- as.integer(gsub("^p", "", p_cols))
  s_lts <- as.integer(gsub("^s", "", s_cols))
  all_lts <- sort(unique(c(p_lts, s_lts)))
  lt_col_names <- paste0("LT", all_lts)

  rows <- list()
  for (i in seq_len(n_configs)) {
    r <- config_rows[i, ]

    # Primera row
    p_row <- tibble(config = config_labels[i], season = "Primera")
    for (lt in all_lts) {
      col_name <- paste0("LT", lt)
      p_col <- paste0("p", lt)

      if (p_col %in% p_cols && !is.na(r[[p_col]])) {
        tp_val <- r[[paste0("p_tp_earliest_lt", lt)]] %||% 0
        fp_val <- r[[paste0("p_fp_marginal_lt", lt)]] %||% 0
        if (lt == 0) {
          p_row[[col_name]] <- sprintf("%dTP / %dFP", tp_val, fp_val)
        } else {
          p_row[[col_name]] <- sprintf("+%dTP / +%dFP", tp_val, fp_val)
        }
      } else {
        p_row[[col_name]] <- "\u2014"
      }
    }

    # Postrera row
    s_row <- tibble(config = config_labels[i], season = "Postrera")
    for (lt in all_lts) {
      col_name <- paste0("LT", lt)
      s_col <- paste0("s", lt)

      if (s_col %in% s_cols && !is.na(r[[s_col]])) {
        tp_val <- r[[paste0("s_tp_earliest_lt", lt)]] %||% 0
        fp_val <- r[[paste0("s_fp_marginal_lt", lt)]] %||% 0
        if (lt == 0) {
          s_row[[col_name]] <- sprintf("%dTP / %dFP", tp_val, fp_val)
        } else {
          s_row[[col_name]] <- sprintf("+%dTP / +%dFP", tp_val, fp_val)
        }
      } else {
        s_row[[col_name]] <- "\u2014"
      }
    }

    rows <- c(rows, list(p_row), list(s_row))
  }

  display <- bind_rows(rows)

  tbl <- display |>
    gt(groupname_col = "config") |>
    cols_label(season = "") |>
    tab_header(
      title = "Leadtime Value: Marginal Early Warnings & False Positives",
      subtitle = "LT0 = base (TP/FP) | LT1+ = marginal contribution (+TP/+FP)"
    ) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) |>
    cols_align(align = "center", columns = -season) |>
    tab_options(table.font.size = px(12), data_row.padding = px(4))

  for (col_name in lt_col_names) {
    tbl <- tbl |>
      tab_style(
        style = cell_fill(color = "#d4edda"),
        locations = cells_body(
          columns = all_of(col_name),
          rows = grepl("\\+[1-9].*\\+0FP", display[[col_name]])
        )
      ) |>
      tab_style(
        style = cell_fill(color = "#f8d7da"),
        locations = cells_body(
          columns = all_of(col_name),
          rows = grepl("\\+0TP.*\\+[1-9]", display[[col_name]])
        )
      ) |>
      tab_style(
        style = cell_fill(color = "#fff3cd"),
        locations = cells_body(
          columns = all_of(col_name),
          rows = grepl("\\+[1-9].*\\+[1-9]", display[[col_name]])
        )
      )
  }

  tbl
}


#' Pareto frontier plot: F1 vs annual RP by LT set
#'
#' Scatterplot of all evaluated configs with mean_f1 on y-axis and annual_rp
#' on x-axis, faceted by LT set (p_lt_count x s_lt_count). Draws the Pareto
#' frontier (configs where you can't improve F1 without worsening RP).
#' Optional vertical reference line at a target RP. Points colored by RP
#' balance (rp_diff).
#'
#' @param df tibble from evaluate_configs(), must have add_lt_set() applied
#' @param target_rp numeric, optional target RP for reference line
#' @param max_rp_display numeric, max RP to show on x-axis (default 10)
#' @param closest_n_target integer, optional. If provided (with target_rp),
#'   zooms x-axis to show only the `n` closest unique RP values to the target.
#' @return list with `plot` (ggplot) and `pareto` (tibble with config_id, thresholds, metrics)
#' @export
plot_pareto_f1_rp <- function(df, target_rp = NULL, max_rp_display = 10,
                               closest_n_target = NULL,
                               y_col = "mean_f1",
                               y_label = NULL) {

  if (is.null(y_label)) {
    y_label <- switch(y_col,
      mean_f1 = "Mean F1 (impact)",
      era5_tol_f1 = "ERA5 Tolerant F1",
      era5_matched_f1 = "ERA5 Matched-RP F1",
      y_col
    )
  }

  # If lt_set columns not present (e.g. combined df), add dummy
  if (!all(c("p_lt_count", "s_lt_count") %in% names(df))) {
    df <- df |> mutate(lt_set = "all")
  } else {
    df <- df |> mutate(lt_set = sprintf("P:%d / S:%d", p_lt_count, s_lt_count))
  }

  df <- df |> filter(annual_rp <= max_rp_display)

  # Zoom to closest N unique RP values around target
  if (!is.null(closest_n_target) && !is.null(target_rp)) {
    unique_rps <- sort(unique(df$annual_rp))
    keep_rps <- unique_rps[order(abs(unique_rps - target_rp))][
      seq_len(min(closest_n_target, length(unique_rps)))
    ]
    df <- df |> filter(annual_rp %in% keep_rps)
  }

  # Use .data pronoun for flexible y column
  top_f1 <- df |>
    group_by(lt_set, annual_rp) |>
    filter(.data[[y_col]] == max(.data[[y_col]])) |>
    ungroup()

  best_per_rp <- top_f1 |>
    group_by(lt_set, annual_rp) |>
    summarise(
      y_val = first(.data[[y_col]]),
      n_configs = n(),
      .groups = "drop"
    )

  # Compute F1 delta relative to target RP
  if (!is.null(target_rp)) {
    target_f1 <- best_per_rp |>
      group_by(lt_set) |>
      slice_min(abs(annual_rp - target_rp), n = 1, with_ties = FALSE) |>
      ungroup() |>
      select(lt_set, target_f1 = y_val, target_annual_rp = annual_rp)

    best_per_rp <- best_per_rp |>
      left_join(target_f1, by = "lt_set") |>
      mutate(
        f1_delta = y_val - target_f1,
        is_target = annual_rp == target_annual_rp,
        rp_label = case_when(
          is_target ~ sprintf("F1=%.2f\nn=%d", y_val, n_configs),
          TRUE ~ sprintf("%+.2f F1\nn=%d", f1_delta, n_configs)
        )
      )
  } else {
    best_per_rp <- best_per_rp |>
      mutate(
        is_target = FALSE,
        rp_label = paste0("n=", n_configs)
      )
  }

  # Check for rp_diff column (not present in combined dfs)
  has_rp_diff <- "rp_diff" %in% names(df)

  p <- ggplot(df, aes(x = annual_rp, y = .data[[y_col]])) +
    {if (has_rp_diff)
      geom_point(aes(color = rp_diff), alpha = 0.5, size = 2, shape = 16)
    else
      geom_point(alpha = 0.5, size = 2, shape = 16, color = "grey50")
    } +
    geom_point(
      data = best_per_rp |> filter(!is_target),
      aes(x = annual_rp, y = y_val),
      color = "#0063B3", size = 1.5
    ) +
    geom_point(
      data = best_per_rp |> filter(is_target),
      aes(x = annual_rp, y = y_val),
      color = "#0063B3", size = 4, shape = 18
    ) +
    geom_label_repel(
      data = best_per_rp,
      aes(x = annual_rp, y = y_val, label = rp_label),
      size = 2.5, color = "#0063B3", fill = "white",
      label.size = 0.2, max.overlaps = 20,
      segment.color = "grey60", segment.size = 0.3,
      min.segment.length = 0
    ) +
    scale_x_continuous(breaks = sort(unique(df$annual_rp)),
                       labels = function(x) sprintf("%.1f", x)) +
    {if (has_rp_diff)
      scale_color_gradient2(
        low = "#1b7837", mid = "#fee08b", high = "#d73027",
        midpoint = 0, name = "RP balance\n(post - pri)")
    } +
    facet_wrap(~lt_set, scales = "free_y") +
    labs(
      x = "Annual RP (years)",
      y = y_label,
      title = sprintf("Best %s vs Annual Return Period", y_label),
      subtitle = "Diamond = target RP | Labels show F1 delta from target & config count"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "right",
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey", fill = NA)
    )

  if (!is.null(target_rp)) {
    p <- p +
      geom_vline(xintercept = target_rp, linetype = "dashed",
                  color = "grey40", linewidth = 0.5)
  }

  list(plot = p, top_f1 = top_f1)
}


#' Summary comparison of configs
#'
#' GT table + dot plot comparing configs across key metrics. Works with any
#' config set — uses `option` column for labels if present, otherwise
#' `config_id`. Designed both for narrowing within a tied set and for
#' final cross-option comparison.
#'
#' @param configs_df tibble of configs (optionally with `option` column)
#' @param trigger_data list from build_trigger_lookup()
#' @return list with `table` (gt) and `plot` (ggplot)
#' @export
plot_option_summary <- function(configs_df, trigger_data) {
  p_cols <- intersect(c("p0", "p1", "p2"), names(configs_df))
  s_cols <- intersect(c("s0", "s1", "s2", "s3"), names(configs_df))

  # Use option column if present, otherwise config_id
  if (!"label" %in% names(configs_df)) {
    configs_df <- configs_df |>
      mutate(label = if ("option" %in% names(configs_df))
        option else paste0("#", config_id))
  }

  # Build display table
  display <- configs_df |>
    rowwise() |>
    mutate(
      primera_thresholds = paste(
        sprintf("%.1f", na.omit(c_across(all_of(p_cols)))), collapse = " / "
      ),
      postrera_thresholds = paste(
        sprintf("%.1f", na.omit(c_across(all_of(s_cols)))), collapse = " / "
      )
    ) |>
    ungroup() |>
    select(
      label,
      primera_thresholds, postrera_thresholds,
      annual_rp, p_seasonal_rp, s_seasonal_rp, rp_diff,
      mean_f1, primera_f1, postrera_f1,
      mean_tol_f1,
      mean_cv, total_early_tp, total_marginal_fp
    )

  tbl <- display |>
    gt() |>
    cols_label(
      label = "",
      primera_thresholds = "Primera RPs",
      postrera_thresholds = "Postrera RPs",
      annual_rp = "Annual RP",
      p_seasonal_rp = "Pri RP",
      s_seasonal_rp = "Post RP",
      rp_diff = "RP Diff",
      mean_f1 = "Mean F1",
      primera_f1 = "Pri F1",
      postrera_f1 = "Post F1",
      mean_tol_f1 = "Tol F1",
      mean_cv = "Mean CV",
      total_early_tp = "Early TPs",
      total_marginal_fp = "Marg FPs"
    ) |>
    fmt_number(columns = c(annual_rp, p_seasonal_rp, s_seasonal_rp, rp_diff),
               decimals = 2) |>
    fmt_number(columns = c(mean_f1, primera_f1, postrera_f1, mean_tol_f1, mean_cv),
               decimals = 3) |>
    tab_spanner(label = "Thresholds", columns = c(primera_thresholds, postrera_thresholds)) |>
    tab_spanner(label = "Return Periods", columns = c(annual_rp, p_seasonal_rp, s_seasonal_rp, rp_diff)) |>
    tab_spanner(label = "Skill (F1)", columns = c(mean_f1, primera_f1, postrera_f1, mean_tol_f1)) |>
    tab_spanner(label = "Diagnostics", columns = c(mean_cv, total_early_tp, total_marginal_fp)) |>
    tab_header(title = "Config Comparison Summary") |>
    data_color(
      columns = mean_f1,
      palette = c("#f8d7da", "#d4edda"),
      domain = range(display$mean_f1)
    ) |>
    data_color(
      columns = mean_tol_f1,
      palette = c("#f8d7da", "#d4edda"),
      domain = range(display$mean_tol_f1)
    ) |>
    data_color(
      columns = mean_cv,
      palette = c("#d4edda", "#f8d7da"),
      domain = range(display$mean_cv)
    ) |>
    data_color(
      columns = total_early_tp,
      palette = c("#f8d7da", "#d4edda"),
      domain = range(display$total_early_tp)
    ) |>
    data_color(
      columns = total_marginal_fp,
      palette = c("#d4edda", "#f8d7da"),
      domain = range(display$total_marginal_fp)
    ) |>
    tab_options(table.font.size = px(11), data_row.padding = px(4))

  # Dot plot of key metrics (normalized to [0,1] for comparability)
  metrics <- configs_df |>
    select(label, annual_rp, mean_f1, mean_tol_f1, mean_cv, total_early_tp, total_marginal_fp) |>
    pivot_longer(-label, names_to = "metric", values_to = "value") |>
    group_by(metric) |>
    mutate(
      better_lower = metric %in% c("mean_cv", "total_marginal_fp"),
      value_norm = (value - min(value)) / (max(value) - min(value) + 1e-9),
      value_norm = if_else(better_lower, 1 - value_norm, value_norm)
    ) |>
    ungroup() |>
    mutate(
      metric_label = case_when(
        metric == "annual_rp" ~ "Annual RP",
        metric == "mean_f1" ~ "Mean F1\n(higher better)",
        metric == "mean_tol_f1" ~ "Tolerant F1\n(higher better)",
        metric == "mean_cv" ~ "Threshold CV\n(lower better)",
        metric == "total_early_tp" ~ "Early TPs\n(higher better)",
        metric == "total_marginal_fp" ~ "Marginal FPs\n(lower better)"
      ),
      label = factor(label, levels = rev(unique(configs_df$label)))
    )

  p <- ggplot(metrics, aes(x = value_norm, y = label, color = label)) +
    geom_point(size = 4) +
    geom_segment(aes(x = 0, xend = value_norm, yend = label), linewidth = 1) +
    geom_text(aes(label = sprintf("%.2f", value)), hjust = -0.3, size = 3) +
    facet_wrap(~metric_label, ncol = 1, scales = "free_x") +
    scale_x_continuous(limits = c(0, 1.3), breaks = c(0, 0.5, 1),
                        labels = c("Worse", "", "Better")) +
    scale_color_brewer(palette = "Set2", guide = "none") +
    labs(x = NULL, y = NULL, title = "Config Comparison: Normalized Metrics") +
    theme_minimal(base_size = 10) +
    theme(
      strip.text = element_text(face = "bold", size = 9),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey", fill = NA)
    )

  list(table = tbl, plot = p)
}


#' Summarise archetypes: unique F1/RP tradeoffs
#'
#' Groups configs by their shared (annual_rp, primera_f1, postrera_f1, mean_f1,
#' p_seasonal_rp, s_seasonal_rp) signature. Each group is an "archetype" —
#' configs that behave identically at the season level but differ in threshold
#' details. Returns a GT table for choosing an archetype and optionally a
#' primera_f1 vs postrera_f1 scatterplot.
#'
#' @param df tibble from filter_top_f1() or similar (top F1 configs)
#' @return list with `table` (gt), `plot` (ggplot), and `archetypes` (tibble
#'   with archetype_id for filtering)
#' @export
summarise_archetypes <- function(df, n_years = NULL, trigger_data = NULL) {
  arch_cols <- c("annual_rp", "p_seasonal_rp", "s_seasonal_rp",
                 "primera_f1", "postrera_f1", "mean_f1", "rp_diff")

  # TP/FP/FN counts are constant within archetype (same F1 + same RP = same counts)
  # so we can safely take first() for these
  has_option <- "option" %in% names(df)
  archetypes <- df |>
    group_by(across(all_of(arch_cols))) |>
    summarise(
      n_configs = n(),
      options = if (has_option) paste(sort(unique(option)), collapse = ", ") else NA_character_,
      primera_tp = first(primera_tp),
      primera_fp = first(primera_fp),
      primera_fn = first(primera_fn),
      postrera_tp = first(postrera_tp),
      postrera_fp = first(postrera_fp),
      postrera_fn = first(postrera_fn),
      primera_tol_f1 = first(primera_tol_f1),
      postrera_tol_f1 = first(postrera_tol_f1),
      mean_tol_f1 = first(mean_tol_f1),
      cv_range = paste0(sprintf("%.3f", min(mean_cv)), " - ",
                         sprintf("%.3f", max(mean_cv))),
      early_tp_range = paste0(min(total_early_tp), " - ", max(total_early_tp)),
      marg_fp_range = paste0(min(total_marginal_fp), " - ", max(total_marginal_fp)),
      .groups = "drop"
    ) |>
    arrange(desc(mean_f1), annual_rp) |>
    mutate(archetype_id = row_number(), .before = 1)

  # Tag original df with archetype_id
  df_tagged <- df |>
    left_join(
      archetypes |> select(all_of(arch_cols), archetype_id),
      by = arch_cols
    )

  # Check within-archetype year agreement if trigger_data provided
  if (!is.null(trigger_data)) {
    tl <- trigger_data$trigger_lookup
    primera_ranked <- trigger_data$primera_ranked
    postrera_ranked <- trigger_data$postrera_ranked
    p_cols <- intersect(c("p0", "p1", "p2"), names(df))
    s_cols <- intersect(c("s0", "s1", "s2", "s3"), names(df))

    # For each config, get season-level trigger years
    get_config_yrs <- function(row) {
      get_yrs <- function(id, val) {
        if (is.na(val)) return(integer(0))
        key <- sprintf("%s_%.4f", id, val)
        tl[[key]] %||% integer(0)
      }
      p <- Reduce(union, lapply(p_cols, function(c) get_yrs(c, row[[c]])))
      s <- Reduce(union, lapply(s_cols, function(c) get_yrs(c, row[[c]])))
      list(primera = sort(p), postrera = sort(s))
    }

    # For each archetype, check if all configs agree on which years trigger
    year_agreement <- df_tagged |>
      group_by(archetype_id) |>
      group_map(function(grp, key) {
        aid <- key$archetype_id
        if (nrow(grp) == 1) {
          return(tibble(archetype_id = aid, year_agreement = "identical (1 config)"))
        }
        all_yrs <- lapply(seq_len(nrow(grp)), function(i) get_config_yrs(grp[i, ]))

        # Check primera
        p_sets <- lapply(all_yrs, `[[`, "primera")
        p_agree <- all(sapply(p_sets[-1], function(x) identical(sort(x), sort(p_sets[[1]]))))

        # Check postrera
        s_sets <- lapply(all_yrs, `[[`, "postrera")
        s_agree <- all(sapply(s_sets[-1], function(x) identical(sort(x), sort(s_sets[[1]]))))

        if (p_agree && s_agree) {
          tibble(archetype_id = aid, year_agreement = "identical")
        } else {
          # Count how many years differ across configs
          all_p <- unique(unlist(p_sets))
          all_s <- unique(unlist(s_sets))
          p_unstable <- sum(sapply(all_p, function(yr)
            length(unique(sapply(p_sets, function(s) yr %in% s))) > 1))
          s_unstable <- sum(sapply(all_s, function(yr)
            length(unique(sapply(s_sets, function(s) yr %in% s))) > 1))
          tibble(
            archetype_id = aid,
            year_agreement = sprintf("%d pri + %d post years differ",
                                      p_unstable, s_unstable)
          )
        }
      }) |>
      bind_rows()

    archetypes <- archetypes |>
      left_join(year_agreement, by = "archetype_id")
  } else {
    archetypes <- archetypes |>
      mutate(year_agreement = NA_character_)
  }

  tbl <- archetypes |>
    gt() |>
    cols_label(
      archetype_id = "#",
      annual_rp = "Annual RP",
      p_seasonal_rp = "Pri RP",
      s_seasonal_rp = "Post RP",
      rp_diff = "RP Diff",
      primera_f1 = "Pri F1",
      postrera_f1 = "Post F1",
      mean_f1 = "Mean F1",
      primera_tp = "TP", primera_fp = "FP", primera_fn = "FN",
      postrera_tp = "TP", postrera_fp = "FP", postrera_fn = "FN",
      options = "Options",
      n_configs = "N Configs",
      year_agreement = "Year Agreement",
      cv_range = "CV Range",
      early_tp_range = "Early TP Range",
      marg_fp_range = "Marg FP Range"
    ) |>
    fmt_number(columns = c(annual_rp, p_seasonal_rp, s_seasonal_rp, rp_diff),
               decimals = 2) |>
    fmt_number(columns = c(primera_f1, postrera_f1, mean_f1), decimals = 3) |>
    tab_spanner(label = "Return Periods",
                columns = c(annual_rp, p_seasonal_rp, s_seasonal_rp, rp_diff)) |>
    cols_label(
      primera_tol_f1 = "Pri Tol F1",
      postrera_tol_f1 = "Post Tol F1",
      mean_tol_f1 = "Mean Tol F1"
    ) |>
    fmt_number(columns = c(primera_tol_f1, postrera_tol_f1, mean_tol_f1),
               decimals = 3) |>
    tab_spanner(label = "Matched F1",
                columns = c(primera_f1, postrera_f1, mean_f1)) |>
    tab_spanner(label = "Tolerant F1",
                columns = c(primera_tol_f1, postrera_tol_f1, mean_tol_f1)) |>
    tab_spanner(label = "Primera",
                columns = c(primera_tp, primera_fp, primera_fn)) |>
    tab_spanner(label = "Postrera",
                columns = c(postrera_tp, postrera_fp, postrera_fn)) |>
    tab_spanner(label = "Within-Archetype Variation",
                columns = c(options, n_configs, year_agreement, cv_range,
                            early_tp_range, marg_fp_range)) |>
    tab_header(
      title = "Archetypes: Unique F1/RP Tradeoffs",
      subtitle = "Each row = configs with identical seasonal performance. Pick an archetype, then drill in."
    ) |>
    data_color(
      columns = mean_f1,
      palette = c("#f8d7da", "#d4edda"),
      domain = range(archetypes$mean_f1)
    ) |>
    data_color(
      columns = mean_tol_f1,
      palette = c("#f8d7da", "#d4edda"),
      domain = range(archetypes$mean_tol_f1)
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = archetype_id)
    ) |>
    tab_options(table.font.size = px(11), data_row.padding = px(4))

  # Build archetype label from options column (already collapsed) or fallback
  archetypes <- archetypes |>
    mutate(arch_label = if_else(
      !is.na(options),
      paste0("#", archetype_id, ": ", options),
      paste0("Archetype #", archetype_id)
    ))

  # Dumbbell chart: primera F1 vs postrera F1 per archetype
  archetypes <- archetypes |>
    mutate(
      arch_label = factor(arch_label, levels = rev(arch_label)),
      rp_annotation = sprintf("RP: %.1f (P:%.1f / S:%.1f)",
                               annual_rp, p_seasonal_rp, s_seasonal_rp)
    )

  p_dumbbell <- ggplot(archetypes, aes(y = arch_label)) +
    # Segment connecting primera and postrera F1
    geom_segment(
      aes(x = primera_f1, xend = postrera_f1, yend = arch_label),
      color = "grey50", linewidth = 1.5
    ) +
    # Primera F1 dot
    geom_point(aes(x = primera_f1, fill = "Primera F1"),
               size = 5, shape = 21, color = "white", stroke = 1) +
    # Postrera F1 dot
    geom_point(aes(x = postrera_f1, fill = "Postrera F1"),
               size = 5, shape = 21, color = "white", stroke = 1) +
    # Mean F1 diamond
    geom_point(aes(x = mean_f1, shape = "Mean F1"),
               size = 4, color = "#0063B3", fill = "#0063B3") +
    # Value labels
    geom_text(aes(x = primera_f1, label = sprintf("%.3f", primera_f1)),
              vjust = -1.2, size = 3, color = "#1b7837") +
    geom_text(aes(x = postrera_f1, label = sprintf("%.3f", postrera_f1)),
              vjust = -1.2, size = 3, color = "#d73027") +
    geom_text(aes(x = mean_f1, label = sprintf("%.3f", mean_f1)),
              vjust = 2, size = 3, fontface = "bold", color = "#0063B3") +
    # RP + n_configs annotation on the right
    geom_text(
      aes(x = max(c(primera_f1, postrera_f1)) + 0.02,
          label = paste0(rp_annotation, "\nn=", n_configs, " configs")),
      hjust = 0, size = 2.8, color = "grey40"
    ) +
    scale_fill_manual(
      name = "Season",
      values = c("Primera F1" = "#1b7837", "Postrera F1" = "#d73027")
    ) +
    scale_shape_manual(name = "", values = c("Mean F1" = 23)) +
    scale_x_continuous(
      expand = expansion(mult = c(0.05, 0.25))
    ) +
    labs(
      x = "F1 Score",
      y = NULL,
      title = "Archetype Comparison: Seasonal Skill Tradeoffs",
      subtitle = "Segment width = seasonal F1 imbalance | Diamond = mean F1"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey", fill = NA),
      legend.position = "bottom",
      axis.text.y = element_text(size = 9, face = "bold")
    )

  # Operational panel: stacked bars showing TP/FP/FN/TN per season per archetype
  # TN = total years - TP - FP - FN
  n_years_val <- if (!is.null(n_years)) {
    n_years
  } else if ("n_p_triggers" %in% names(df)) {
    round(df$p_seasonal_rp[1] * df$n_p_triggers[1])
  } else {
    stop("Cannot derive n_years. Pass it explicitly via n_years argument.")
  }

  archetypes <- archetypes |>
    mutate(
      primera_tn = n_years_val - primera_tp - primera_fp - primera_fn,
      postrera_tn = n_years_val - postrera_tp - postrera_fp - postrera_fn
    )

  confusion_long <- archetypes |>
    select(arch_label, primera_tp, primera_fp, primera_fn, primera_tn,
           postrera_tp, postrera_fp, postrera_fn, postrera_tn) |>
    pivot_longer(
      cols = -arch_label,
      names_to = c("season", "outcome"),
      names_pattern = "(primera|postrera)_(tp|fp|fn|tn)",
      values_to = "count"
    ) |>
    mutate(
      season = factor(
        ifelse(season == "primera", "Primera", "Postrera"),
        levels = c("Primera", "Postrera")
      ),
      outcome = factor(
        toupper(outcome),
        levels = c("TP", "FP", "FN", "TN")
      ),
      text_color = ifelse(outcome %in% c("TN", "FN"), "grey30", "white")
    )

  confusion_colors <- c("TP" = "#2ca02c", "FP" = "#d62728",
                         "FN" = "#d0d0d0", "TN" = "#E0EBF5")

  p_confusion <- ggplot(confusion_long,
                        aes(x = count, y = arch_label, fill = outcome)) +
    geom_col(position = "stack", width = 0.6) +
    geom_text(
      aes(label = ifelse(count > 0, count, ""), color = text_color,
          group = outcome),
      position = position_stack(vjust = 0.5),
      size = 3, fontface = "bold", show.legend = FALSE
    ) +
    scale_color_identity() +
    facet_wrap(~season, ncol = 2) +
    scale_fill_manual(values = confusion_colors, name = "Outcome") +
    labs(
      x = "Number of Years",
      y = NULL,
      title = "Operational Breakdown: Hit/Miss per Season",
      subtitle = "TP = triggered droughts | FP = false alarms | FN = missed | TN = correct no-action"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey", fill = NA),
      legend.position = "bottom",
      axis.text.y = element_text(size = 9, face = "bold"),
      strip.text = element_text(face = "bold")
    )

  p_combined <- p_dumbbell / p_confusion +
    plot_layout(heights = c(1, 1))

  # One representative config per archetype for cross-archetype comparison
  representatives <- df_tagged |>
    group_by(archetype_id) |>
    slice_head(n = 1) |>
    ungroup() |>
    left_join(
      archetypes |> select(archetype_id, arch_label),
      by = "archetype_id"
    )

  list(table = tbl, plot = p_combined,
       plot_f1 = p_dumbbell, plot_confusion = p_confusion,
       archetypes = df_tagged, representatives = representatives)
}


#' Impact validation ground truth table
#'
#' GT table showing year-by-year alignment of CERF allocations, EM-DAT
#' total_affected, and ERA5 seasonal dry ranks for one country. Uses
#' inline bar charts for EM-DAT affected and ERA5 ranks.
#'
#' @param df_joined data.frame with year, window, obs_mm (from load_country_data)
#' @param df_cerf_drought tibble with iso3, year columns (drought CERF allocations)
#' @param df_emdat_drought tibble with iso3, year, total_affected columns
#' @param iso3_code character, e.g. "HND"
#' @param eval_start integer, first year (default 1991)
#' @param eval_end integer, last year (default 2024)
#' @param df_emdat_interp optional tibble with iso3, year, total_affected_interp
#'   columns. When EM-DAT events span multiple years (start_year != end_year),
#'   total_affected is divided equally across all years in the range.
#' @param sort_by character, column to sort by (descending for numeric cols,
#'   ascending for "year"). Available columns:
#'   - "year": chronological order (ascending)
#'   - "total_affected": EM-DAT reported affected (desc)
#'   - "total_affected_interp": EM-DAT interpolated affected (desc, requires df_emdat_interp)
#'   - "primera_rp": ERA5 primera obs RP (desc, highest RP first)
#'   - "postrera_rp": ERA5 postrera obs RP (desc)
#'   - "max_rp": max of primera/postrera RP (desc)
#'   - "primera_cum_rp": cumulative primera RP (desc)
#'   - "postrera_cum_rp": cumulative postrera RP (desc)
#'   - "max_cum_rp": max of cumulative primera/postrera RP (desc)
#' @return gt table object
#' @export
build_impact_validation_gt <- function(df_joined, df_cerf_drought, df_emdat_drought,
                                        iso3_code, eval_start = 1991, eval_end = 2024,
                                        df_emdat_interp = NULL,
                                        cerf_hypothesis_years = NULL,
                                        sort_by = "total_affected") {
  years <- eval_start:eval_end

  # ERA5 dry ranks per season (rank 1 = driest)
  obs_by_year <- df_joined |>
    filter(year >= eval_start, year <= eval_end) |>
    distinct(year, window, obs_mm)

  primera_rps <- obs_by_year |>
    filter(window == "primera") |>
    mutate(
      dry_rank = rank(obs_mm, ties.method = "first"),
      primera_rp = (n() + 1) / dry_rank
    ) |>
    select(year, primera_rp)

  postrera_rps <- obs_by_year |>
    filter(window == "postrera") |>
    mutate(
      dry_rank = rank(obs_mm, ties.method = "first"),
      postrera_rp = (n() + 1) / dry_rank
    ) |>
    select(year, postrera_rp)

  n_years <- nrow(primera_rps)

  # CERF: country-specific and regional (any dry corridor country)
  cerf_country <- df_cerf_drought |>
    filter(iso3 == iso3_code, year >= eval_start, year <= eval_end) |>
    distinct(year) |>
    mutate(cerf_country = TRUE)

  cerf_regional <- df_cerf_drought |>
    filter(year >= eval_start, year <= eval_end) |>
    distinct(year) |>
    mutate(cerf_regional = TRUE)

  # CERF hypothesis years (optional)
  has_hyp <- !is.null(cerf_hypothesis_years) && length(cerf_hypothesis_years) > 0
  if (has_hyp) {
    cerf_hyp <- tibble(
      year = as.integer(cerf_hypothesis_years),
      cerf_hyp = TRUE
    ) |> filter(year >= eval_start, year <= eval_end)
  }

  # EM-DAT: total affected
  emdat_data <- df_emdat_drought |>
    filter(iso3 == iso3_code, year >= eval_start, year <= eval_end) |>
    select(year, total_affected)

  # EM-DAT interpolated (optional)
  has_interp <- !is.null(df_emdat_interp)
  if (has_interp) {
    emdat_interp_data <- df_emdat_interp |>
      filter(iso3 == iso3_code, year >= eval_start, year <= eval_end) |>
      select(year, total_affected_interp)
  }

  # Assemble
  tbl_data <- tibble(year = years) |>
    left_join(cerf_country, by = "year") |>
    left_join(cerf_regional, by = "year") |>
    left_join(emdat_data, by = "year") |>
    left_join(primera_rps, by = "year") |>
    left_join(postrera_rps, by = "year") |>
    mutate(
      cerf_country = replace_na(cerf_country, FALSE),
      cerf_regional = replace_na(cerf_regional, FALSE),
      cerf_c = if_else(cerf_country, "\u2713", ""),
      cerf_r = if_else(cerf_regional, "\u2713", ""),
      max_rp = pmax(primera_rp, postrera_rp, na.rm = TRUE)
    )

  if (has_hyp) {
    tbl_data <- tbl_data |>
      left_join(cerf_hyp, by = "year") |>
      mutate(
        cerf_hyp = replace_na(cerf_hyp, FALSE),
        cerf_h = if_else(cerf_hyp, "\u2713", "")
      )
  }

  if (has_interp) {
    tbl_data <- tbl_data |> left_join(emdat_interp_data, by = "year")
  }

  max_affected <- max(tbl_data$total_affected, na.rm = TRUE)
  if (is.infinite(max_affected) || is.na(max_affected)) max_affected <- 1
  # Use same max for both bars so they're on the same scale
  if (has_interp) {
    max_bar <- max(max_affected,
                   max(tbl_data$total_affected_interp, na.rm = TRUE),
                   na.rm = TRUE)
  } else {
    max_bar <- max_affected
  }

  # Sort by chosen column (desc for numeric, asc for year)
  # sort_by options: "year", "total_affected", "total_affected_interp",
  #                  "primera_rp", "postrera_rp", "max_rp"
  if (sort_by == "year") {
    display <- tbl_data |> arrange(year)
  } else {
    display <- tbl_data |>
      arrange(desc(replace_na(.data[[sort_by]], -Inf)), year)
  }

  cerf_cols <- if (has_hyp) c("cerf_c", "cerf_r", "cerf_h") else c("cerf_c", "cerf_r")

  if (has_interp) {
    display <- display |>
      select(year, all_of(cerf_cols), total_affected, total_affected_interp,
             primera_rp, postrera_rp, max_rp)
  } else {
    display <- display |>
      select(year, all_of(cerf_cols), total_affected,
             primera_rp, postrera_rp, max_rp)
  }

  # Compact number label (1.3M, 47K, 800)
  fmt_compact <- function(v) {
    if (v >= 1e6) sprintf("%.1fM", v / 1e6)
    else if (v >= 1e3) sprintf("%.0fK", v / 1e3)
    else as.character(as.integer(v))
  }

  # Track row order for conditional styling
  is_cerf_c <- display$year %in% tbl_data$year[tbl_data$cerf_country]
  is_cerf_r <- display$year %in% tbl_data$year[tbl_data$cerf_regional]
  if (has_hyp) {
    is_cerf_h <- display$year %in% tbl_data$year[tbl_data$cerf_hyp]
  }

  # Inline bar with overlaid label
  make_bar_transform <- function(col_name, color, max_val) {
    text_transform_fn <- function(x) {
      vals <- suppressWarnings(as.numeric(x))
      lapply(vals, function(v) {
        if (is.na(v)) return(html(""))
        pct <- max(v / max_val * 100, 3)
        lbl <- fmt_compact(v)
        txt_col <- if (pct > 25) "white" else "#333"
        html(sprintf(
          paste0(
            '<div style="position:relative;width:100%%;height:16px;">',
            '<div style="background:%s;height:100%%;width:%.0f%%;',
            'border-radius:2px;"></div>',
            '<span style="position:absolute;left:4px;top:0;',
            'font-size:10px;line-height:16px;font-weight:bold;',
            'color:%s;white-space:nowrap;">%s</span></div>'
          ),
          color, pct, txt_col, lbl
        ))
      })
    }
    text_transform_fn
  }

  # RP color domain
  max_rp_val <- max(display$max_rp, na.rm = TRUE)
  if (is.infinite(max_rp_val) || is.na(max_rp_val)) max_rp_val <- n_years

  rp_cols <- c("primera_rp", "postrera_rp", "max_rp")

  cerf_label_list <- list(cerf_c = "Country", cerf_r = "Regional")
  if (has_hyp) cerf_label_list[["cerf_h"]] <- "Hypothesis"

  tbl <- display |>
    gt() |>
    cols_label(
      year = "Year",
      !!!cerf_label_list,
      total_affected = "Reported",
      primera_rp = "Pri",
      postrera_rp = "Post",
      max_rp = "Max"
    ) |>
    fmt_number(columns = all_of(rp_cols), decimals = 1) |>
    # CERF: green highlight
    tab_style(
      style = cell_fill(color = "#d4edda"),
      locations = cells_body(columns = cerf_c, rows = is_cerf_c)
    ) |>
    tab_style(
      style = cell_fill(color = "#d4edda"),
      locations = cells_body(columns = cerf_r, rows = is_cerf_r)
    ) |>
    # EM-DAT reported: inline bar
    text_transform(
      locations = cells_body(columns = total_affected),
      fn = make_bar_transform("total_affected", "#de2d26", max_bar)
    ) |>
    # ERA5 obs RP: brown gradient (higher RP = darker)
    sub_missing(columns = all_of(rp_cols), missing_text = "") |>
    data_color(
      columns = all_of(rp_cols),
      palette = c("#FDEBD0", "#8B4513"),
      domain = c(1, max_rp_val),
      na_color = "white"
    ) |>
    tab_spanner(label = "CERF", columns = all_of(cerf_cols)) |>
    tab_spanner(label = "ERA5 Obs RP", columns = all_of(rp_cols))

  # Add interpolated column if provided
  if (has_interp) {
    tbl <- tbl |>
      cols_label(total_affected_interp = "Interpolated") |>
      text_transform(
        locations = cells_body(columns = total_affected_interp),
        fn = make_bar_transform("total_affected_interp", "#e6550d", max_bar)
      ) |>
      tab_spanner(
        label = "EM-DAT Affected",
        columns = c(total_affected, total_affected_interp)
      )
  } else {
    tbl <- tbl |>
      cols_label(total_affected = "EM-DAT Affected")
  }

  # CERF hypothesis: pink fill (matches impact summary P1 color)
  if (has_hyp) {
    tbl <- tbl |>
      tab_style(
        style = cell_fill(color = "#fde0dd"),
        locations = cells_body(columns = cerf_h, rows = is_cerf_h)
      )
  }

  tbl <- tbl |>
    tab_header(
      title = sprintf("Impact Validation: %s (%d\u2013%d)", iso3_code, eval_start, eval_end),
      subtitle = paste0("Sorted by ", sort_by, " (desc)")
    ) |>
    cols_align(
      align = "center",
      columns = c(all_of(cerf_cols), all_of(rp_cols))
    ) |>
    cols_width(
      total_affected ~ px(150),
      any_of("total_affected_interp") ~ px(150)
    ) |>
    tab_options(table.font.size = px(11), data_row.padding = px(2))

  tbl
}


#' ROC-AUC analysis: ERA5 seasonal RP as predictor of EM-DAT drought impact
#'
#' Produces 3 ROC curves side-by-side: primera, postrera, and annual (empirical
#' combined). Per-season curves use seasonal empirical RP as predictor. Annual
#' curve uses max(primera_rp, postrera_rp) ranked empirically — no independence
#' assumption. Binary truth = EM-DAT drought record exists (regardless of
#' total_affected value).
#'
#' @param df_joined data.frame with year, window, obs_mm (from load_country_data)
#' @param df_emdat_drought tibble with iso3, year columns (drought events from EM-DAT)
#' @param iso3_code character, e.g. "HND"
#' @param eval_start integer, first year (default 1991)
#' @param eval_end integer, last year (default 2024)
#' @return list with components: roc_primera, roc_postrera, roc_annual (each a
#'   pROC roc object), plot (patchwork of 3 panels), best_thresholds (tibble
#'   with season, threshold, sensitivity, specificity, auc), roc_data
#' @export
plot_impact_roc <- function(df_joined, df_emdat_drought,
                            iso3_code, eval_start = 1991, eval_end = 2024) {

  # ERA5 obs → empirical RP per season
  obs_by_year <- df_joined |>
    filter(year >= eval_start, year <= eval_end) |>
    distinct(year, window, obs_mm)

  primera_rps <- obs_by_year |>
    filter(window == "primera") |>
    mutate(
      dry_rank = rank(obs_mm, ties.method = "first"),
      primera_rp = (n() + 1) / dry_rank
    ) |>
    select(year, primera_rp)

  postrera_rps <- obs_by_year |>
    filter(window == "postrera") |>
    mutate(
      dry_rank = rank(obs_mm, ties.method = "first"),
      postrera_rp = (n() + 1) / dry_rank
    ) |>
    select(year, postrera_rp)

  n_years <- length(eval_start:eval_end)

  # Binary truth: EM-DAT drought record exists for this country-year
  emdat_years <- df_emdat_drought |>
    filter(iso3 == iso3_code, year >= eval_start, year <= eval_end) |>
    pull(year) |>
    unique()

  # Assemble data
  roc_data <- tibble(year = eval_start:eval_end) |>
    left_join(primera_rps, by = "year") |>
    left_join(postrera_rps, by = "year") |>
    mutate(
      max_rp = pmax(primera_rp, postrera_rp, na.rm = TRUE),
      annual_rank = rank(-max_rp, ties.method = "first"),
      annual_rp = (n_years + 1) / annual_rank,
      impact = year %in% emdat_years
    )

  # Helper: build one ROC panel
  build_roc_panel <- function(predictor_col, label, color) {
    roc_obj <- roc(roc_data$impact, roc_data[[predictor_col]],
                   direction = "<", quiet = TRUE)
    auc_val <- auc(roc_obj)
    auc_ci <- ci.auc(roc_obj, method = "delong")
    best <- coords(roc_obj, "best",
                   ret = c("threshold", "sensitivity", "specificity"),
                   best.method = "youden")

    roc_df <- tibble(
      fpr = 1 - roc_obj$specificities,
      tpr = roc_obj$sensitivities
    )

    p <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
      geom_line(color = color, linewidth = 1) +
      geom_abline(linetype = "dashed", color = "grey50") +
      geom_point(
        data = tibble(fpr = 1 - best$specificity, tpr = best$sensitivity),
        size = 3, color = color
      ) +
      annotate(
        "text", x = 0.5, y = 0.2,
        label = sprintf(
          "AUC = %.2f [%.2f, %.2f]\nOptimal RP >= %.1f\nSens = %.0f%%, Spec = %.0f%%",
          auc_val, auc_ci[1], auc_ci[3],
          best$threshold, best$sensitivity * 100, best$specificity * 100
        ),
        hjust = 0, size = 3, lineheight = 1
      ) +
      labs(title = label, x = "FPR", y = "TPR") +
      coord_equal() +
      theme_minimal(base_size = 10)

    list(roc_obj = roc_obj, plot = p, best = best, auc = auc_val)
  }

  # Build 3 panels
  p_res <- build_roc_panel("primera_rp", "Primera", "#1b9e77")
  s_res <- build_roc_panel("postrera_rp", "Postrera", "#d95f02")
  a_res <- build_roc_panel("annual_rp", "Annual (either)", "#de2d26")

  # Combine with patchwork
  combined <- p_res$plot + s_res$plot + a_res$plot +
    patchwork::plot_annotation(
      title = sprintf("ROC: ERA5 Obs RP \u2192 EM-DAT Drought Impact (%s)", iso3_code),
      subtitle = sprintf(
        "%d\u2013%d | %d impact years out of %d",
        eval_start, eval_end, length(emdat_years), n_years
      )
    )

  # Summary table of optimal thresholds
  best_thresholds <- tibble(
    season = c("primera", "postrera", "annual"),
    threshold = c(p_res$best$threshold, s_res$best$threshold, a_res$best$threshold),
    sensitivity = c(p_res$best$sensitivity, s_res$best$sensitivity, a_res$best$sensitivity),
    specificity = c(p_res$best$specificity, s_res$best$specificity, a_res$best$specificity),
    auc = c(as.numeric(p_res$auc), as.numeric(s_res$auc), as.numeric(a_res$auc))
  )

  list(
    roc_primera = p_res$roc_obj,
    roc_postrera = s_res$roc_obj,
    roc_annual = a_res$roc_obj,
    plot = combined,
    best_thresholds = best_thresholds,
    roc_data = roc_data
  )
}


#' Seasonal rainfall anomaly bar chart with EM-DAT drought markers
#'
#' Dodged bar chart of primera/postrera rainfall anomalies (% departure from
#' mean) per year, faceted by country. EM-DAT drought years (expanded across
#' start_year:end_year) are marked with stars above the bars.
#'
#' @param df_obs_list named list of data frames from load_country_data(),
#'   keyed by ISO3 code (e.g. list(HND = df_hnd, GTM = df_gtm, SLV = df_slv))
#' @param df_emdat_impact tibble with iso3, year columns (expanded EM-DAT
#'   drought years, where multi-year events span start_year:end_year)
#' @param eval_start integer, first year (default 1991)
#' @param eval_end integer, last year (default 2024)
#' @return ggplot object
#' @export
plot_seasonal_anomaly <- function(df_obs_list, df_emdat_impact,
                                  eval_start = 1991, eval_end = 2024) {

  # Build combined obs anomaly data across all countries
  obs_all <- map_dfr(names(df_obs_list), function(iso) {
    df <- df_obs_list[[iso]]
    df |>
      filter(year >= eval_start, year <= eval_end) |>
      distinct(year, window, obs_mm) |>
      group_by(window) |>
      mutate(
        mean_mm = mean(obs_mm),
        anomaly_pct = (obs_mm - mean_mm) / mean_mm * 100
      ) |>
      ungroup() |>
      mutate(iso3 = iso)
  })

  # EM-DAT: country-level drought flag
  emdat_yrs <- df_emdat_impact |>
    filter(year >= eval_start, year <= eval_end) |>
    distinct(iso3, year) |>
    mutate(emdat = TRUE)

  plot_data <- obs_all |>
    left_join(emdat_yrs, by = c("iso3", "year")) |>
    mutate(
      emdat = replace_na(emdat, FALSE),
      window = factor(window, levels = c("primera", "postrera")),
      year_label = sprintf("'%02d", year %% 100)
    )

  # Stars at fixed vertical position below bars for EM-DAT years
  y_ranges <- plot_data |>
    group_by(iso3) |>
    summarise(y_min = min(anomaly_pct, na.rm = TRUE), .groups = "drop")

  # One star per year-country (not per season) — centered on the year
  star_data <- plot_data |>
    filter(emdat) |>
    distinct(iso3, year) |>
    left_join(y_ranges, by = "iso3") |>
    mutate(star_y = y_min - 5)

  p <- ggplot(plot_data, aes(x = year, y = anomaly_pct, fill = window)) +
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.7,
      color = NA
    ) +
    # Filled markers at fixed y position for EM-DAT years
    geom_point(
      data = star_data,
      aes(x = year, y = star_y),
      inherit.aes = FALSE,
      shape = 17, size = 2.5, color = "#e41a1c",
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = c("primera" = "#1b9e77", "postrera" = "#d95f02"),
      name = "Season"
    ) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    scale_x_continuous(
      breaks = eval_start:eval_end,
      labels = sprintf("'%02d", (eval_start:eval_end) %% 100)
    ) +
    facet_wrap(~iso3, ncol = 1, scales = "free_y") +
    labs(
      title = "ERA5 Seasonal Rainfall Anomaly",
      subtitle = sprintf(
        "%d\u2013%d | \u25b2 = EM-DAT drought year (country-level, expanded)",
        eval_start, eval_end
      ),
      x = NULL,
      y = "Anomaly (% of mean)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
    )

  p
}

#' Scatter plot of primera vs postrera anomaly by year
#'
#' Each point is a year, colored by EM-DAT status.
#' Year labels on points. Faceted by country.
#'
#' @param df_obs_list Named list (iso3 → trigger_data from build_trigger_lookup)
#' @param df_emdat_impact Tibble with iso3, year columns for drought events
#' @param eval_start,eval_end Integer year bounds
#' @return ggplot
#' @export
plot_seasonal_scatter <- function(df_obs_list, df_emdat_impact,
                                  eval_start = 1991, eval_end = 2024) {

  # Build wide obs data: one row per country-year with primera & postrera anomaly
  obs_all <- map_dfr(names(df_obs_list), function(iso) {
    df <- df_obs_list[[iso]]
    df |>
      filter(year >= eval_start, year <= eval_end) |>
      distinct(year, window, obs_mm) |>
      group_by(window) |>
      mutate(
        mean_mm = mean(obs_mm),
        anomaly_pct = (obs_mm - mean_mm) / mean_mm * 100
      ) |>
      ungroup() |>
      mutate(iso3 = iso)
  })

  obs_wide <- obs_all |>
    select(iso3, year, window, anomaly_pct) |>
    tidyr::pivot_wider(names_from = window, values_from = anomaly_pct)

  # EM-DAT flag
  emdat_yrs <- df_emdat_impact |>
    filter(year >= eval_start, year <= eval_end) |>
    distinct(iso3, year) |>
    mutate(emdat = TRUE)

  plot_data <- obs_wide |>
    left_join(emdat_yrs, by = c("iso3", "year")) |>
    mutate(
      emdat = replace_na(emdat, FALSE),
      year_label = sprintf("'%02d", year %% 100)
    )

  p <- ggplot(plot_data, aes(x = primera, y = postrera)) +
    geom_hline(yintercept = 0, linewidth = 0.3, color = "grey50") +
    geom_vline(xintercept = 0, linewidth = 0.3, color = "grey50") +
    geom_point(
      aes(color = emdat),
      size = 2.5
    ) +
    ggrepel::geom_text_repel(
      aes(label = year_label, color = emdat),
      size = 3, max.overlaps = 30,
      show.legend = FALSE
    ) +
    scale_color_manual(
      values = c("FALSE" = "grey50", "TRUE" = "#e41a1c"),
      labels = c("FALSE" = "No EM-DAT record", "TRUE" = "EM-DAT drought"),
      name = NULL
    ) +
    facet_wrap(~iso3, scales = "free") +
    labs(
      title = "Primera vs Postrera Rainfall Anomaly",
      subtitle = sprintf(
        "%d\u2013%d | Red = EM-DAT drought year",
        eval_start, eval_end
      ),
      x = "Primera anomaly (% of mean)",
      y = "Postrera anomaly (% of mean)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "top"
    )

  p
}

#' Scatter: ERA5 tolerant F1 vs Impact F1, colored by annual RP
#'
#' Highlights configs near a target RP with distinct colors. RPs far from
#' target are muted grey; the nearest RP levels above and below target
#' get bright, high-contrast colors.
#'
#' @param df Combined dataframe with era5_tol_f1, impact_f1, annual_rp
#' @param target_rp Numeric target RP (used to center the color scale)
#' @param n_nearest Integer, how many RP levels above/below target to highlight (default 2)
#' @return ggplot
#' @export
plot_f1_tradeoff <- function(df, target_rp = 3, n_nearest = 2) {

  unique_rps <- sort(unique(df$annual_rp))

  # Find the n nearest RPs below and above target
  below <- tail(unique_rps[unique_rps <= target_rp], n_nearest)
  above <- head(unique_rps[unique_rps > target_rp], n_nearest)
  highlight_rps <- c(below, above)

  # Assign discrete labels: highlighted RPs get their value, others -> "other"
  df <- df |>
    mutate(
      rp_group = if_else(annual_rp %in% highlight_rps,
                         sprintf("RP %.1f", annual_rp), "other"),
      rp_group = factor(rp_group,
                        levels = c(sprintf("RP %.1f", sort(highlight_rps)), "other"))
    )

  # Bright distinct palette for highlighted RPs, grey for rest
  n_highlight <- length(highlight_rps)
  highlight_colors <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3",
                        "#ff7f00", "#a65628")[seq_len(n_highlight)]
  all_colors <- c(setNames(highlight_colors, sprintf("RP %.1f", sort(highlight_rps))),
                  "other" = "grey80")

  # Average F1 across both metrics
  df <- df |> mutate(avg_f1 = (era5_tol_f1 + impact_f1) / 2)

  df_highlight <- df |> filter(rp_group != "other")
  df_bg <- df |> filter(rp_group == "other")

  # Best avg F1 per highlighted RP group (for circled points)
  df_best <- df_highlight |>
    group_by(rp_group) |>
    filter(avg_f1 == max(avg_f1)) |>
    ungroup()

  # One label per RP group with n count
  df_best_labels <- df_best |>
    group_by(rp_group, avg_f1) |>
    summarise(
      era5_tol_f1 = mean(era5_tol_f1),
      impact_f1 = mean(impact_f1),
      n = n(),
      .groups = "drop"
    )

  p <- ggplot(mapping = aes(x = era5_tol_f1, y = impact_f1)) +
    geom_point(data = df_bg, color = "grey80", size = 1.5, alpha = 0.4) +
    geom_point(data = df_highlight, aes(color = rp_group), size = 2.5, alpha = 0.8) +
    # Circle around best avg F1 per RP group
    geom_point(data = df_best, aes(color = rp_group),
               size = 5, shape = 1, stroke = 1.2, show.legend = FALSE) +
    geom_label_repel(
      data = df_best_labels,
      aes(label = sprintf("avg=%.2f\nn=%d", avg_f1, n), color = rp_group),
      size = 2.8, fill = "white", label.size = 0.2,
      show.legend = FALSE, max.overlaps = 30,
      segment.color = "grey50", segment.size = 0.3,
      min.segment.length = 0, force = 5,
      nudge_x = 0.01
    ) +
    scale_color_manual(values = all_colors, name = "Annual RP") +
    labs(
      x = "ERA5 Tolerant F1",
      y = "Impact F1 (EM-DAT)",
      title = "ERA5 vs Impact F1 Tradeoff",
      subtitle = sprintf(
        "Target RP \u2248 %.1f | Circles = best avg(ERA5, Impact) F1 per RP",
        target_rp)
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank()
    )

  list(plot = p, best_per_rp = df_best, data = df)
}


#' GT table showing trigger activation per leadtime for a given config
#'
#' Similar layout to build_impact_validation_gt but replaces cumulative RP
#' columns with per-leadtime forecast empirical RP values. Cells are highlighted
#' green when the trigger fires at that leadtime (forecast RP >= threshold RP).
#'
#' @param config_row single-row tibble with threshold columns (p0, p1, ...)
#' @param trigger_data list from build_trigger_lookup()
#' @param df_joined data.frame with year, window, leadtime, fcst_mm, obs_mm
#' @param df_cerf_drought tibble with iso3, year
#' @param df_emdat_drought tibble with iso3, year, total_affected
#' @param iso3_code character e.g. "HND"
#' @param eval_start integer
#' @param eval_end integer
#' @param df_emdat_interp optional tibble with iso3, year, total_affected_interp
#' @param must_hit_years integer vector of P1 years
#' @param should_hit_years integer vector of P2 years
#' @param sort_by column to sort by (default "year")
#' @param lt_labels named character vector mapping LT ids to display labels
#' @return gt table
#' @export
build_trigger_activation_gt <- function(config_row, trigger_data, df_joined,
                                        df_cerf_drought, df_emdat_drought,
                                        iso3_code, eval_start = 1991, eval_end = 2024,
                                        df_emdat_interp = NULL,
                                        must_hit_years = integer(0),
                                        should_hit_years = integer(0),
                                        sort_by = "year",
                                        lt_labels = NULL) {
  years <- eval_start:eval_end
  tl <- trigger_data$trigger_lookup

  if (is.null(lt_labels)) {
    lt_labels <- c(
      p0 = "LT0 (May)", p1 = "LT1 (Apr)", p2 = "LT2 (Mar)",
      s0 = "LT0 (Sep)", s1 = "LT1 (Aug)", s2 = "LT2 (Jul)", s3 = "LT3 (Jun)"
    )
  }

  # Detect active LT columns from config (non-NA thresholds)
  all_possible <- c("p0", "p1", "p2", "s0", "s1", "s2", "s3")
  active_lts <- character(0)
  for (col in all_possible) {
    if (col %in% names(config_row) && !is.na(config_row[[col]])) {
      active_lts <- c(active_lts, col)
    }
  }
  p_lts <- active_lts[startsWith(active_lts, "p")]
  s_lts <- active_lts[startsWith(active_lts, "s")]

  # Get trigger years per LT
  trigger_yrs_per_lt <- setNames(
    lapply(active_lts, function(col) {
      key <- sprintf("%s_%.4f", col, config_row[[col]])
      yrs <- tl[[key]]
      if (is.null(yrs)) return(integer(0))
      yrs[yrs >= eval_start & yrs <= eval_end]
    }),
    active_lts
  )

  # Forecast empirical RP per year per LT (computed from full dataset)
  fcst_rp_wide <- df_joined |>
    group_by(window, leadtime) |>
    mutate(
      dry_rank = rank(fcst_mm, ties.method = "first"),
      fcst_rp = (n() + 1) / dry_rank
    ) |>
    ungroup() |>
    mutate(lt_id = paste0(if_else(window == "primera", "p", "s"), leadtime)) |>
    filter(lt_id %in% active_lts, year >= eval_start, year <= eval_end) |>
    select(year, lt_id, fcst_rp) |>
    pivot_wider(names_from = lt_id, values_from = fcst_rp, names_prefix = "rp_")

  # ERA5 obs RP per season
  obs_by_year <- df_joined |>
    filter(year >= eval_start, year <= eval_end) |>
    distinct(year, window, obs_mm)

  primera_rps <- obs_by_year |>
    filter(window == "primera") |>
    mutate(dry_rank = rank(obs_mm, ties.method = "first"),
           obs_rp_primera = (n() + 1) / dry_rank) |>
    select(year, obs_rp_primera)

  postrera_rps <- obs_by_year |>
    filter(window == "postrera") |>
    mutate(dry_rank = rank(obs_mm, ties.method = "first"),
           obs_rp_postrera = (n() + 1) / dry_rank) |>
    select(year, obs_rp_postrera)

  n_years <- nrow(primera_rps)

  # Impact data
  cerf_country <- df_cerf_drought |>
    filter(iso3 == iso3_code, year >= eval_start, year <= eval_end) |>
    distinct(year) |>
    mutate(cerf = TRUE)

  emdat_data <- df_emdat_drought |>
    filter(iso3 == iso3_code, year >= eval_start, year <= eval_end) |>
    select(year, total_affected)

  has_interp <- !is.null(df_emdat_interp)
  if (has_interp) {
    emdat_interp_data <- df_emdat_interp |>
      filter(iso3 == iso3_code, year >= eval_start, year <= eval_end) |>
      select(year, total_affected_interp)
  }

  # Assemble
  tbl_data <- tibble(year = years) |>
    left_join(cerf_country, by = "year") |>
    left_join(emdat_data, by = "year") |>
    left_join(fcst_rp_wide, by = "year") |>
    left_join(primera_rps, by = "year") |>
    left_join(postrera_rps, by = "year") |>
    mutate(
      cerf = replace_na(cerf, FALSE),
      cerf_label = if_else(cerf, "\u2713", ""),
      hit_priority = case_when(
        year %in% must_hit_years ~ 1L,
        year %in% should_hit_years ~ 2L,
        TRUE ~ NA_integer_
      ),
      max_obs_rp = pmax(obs_rp_primera, obs_rp_postrera, na.rm = TRUE)
    )

  if (has_interp) {
    tbl_data <- tbl_data |> left_join(emdat_interp_data, by = "year")
  }

  # Mark triggered per LT
  for (col in active_lts) {
    trig_col <- paste0("trig_", col)
    tbl_data[[trig_col]] <- tbl_data$year %in% trigger_yrs_per_lt[[col]]
  }

  # Annual activation: TRUE if any LT triggered for this year
  trig_cols_all <- paste0("trig_", active_lts)
  tbl_data <- tbl_data |>
    rowwise() |>
    mutate(annual_activated = any(c_across(all_of(trig_cols_all)))) |>
    ungroup()

  # Sort
  if (sort_by == "activation") {
    display <- tbl_data |> arrange(desc(annual_activated), year)
  } else if (sort_by == "year") {
    display <- tbl_data |> arrange(year)
  } else {
    display <- tbl_data |>
      arrange(desc(replace_na(.data[[sort_by]], -Inf)), year)
  }

  # Column order: LT0 on left, increasing to right (p0, p1, p2, s0, s1, s2)
  p_lts_ordered <- sort(p_lts)
  s_lts_ordered <- sort(s_lts)
  active_lts_ordered <- c(p_lts_ordered, s_lts_ordered)
  rp_cols <- paste0("rp_", active_lts_ordered)
  trig_cols <- paste0("trig_", active_lts)
  obs_cols <- c("obs_rp_primera", "obs_rp_postrera")

  # Select columns for display
  display_cols <- c("year", "hit_priority", "cerf_label")
  if (has_interp) {
    display_cols <- c(display_cols, "total_affected", "total_affected_interp")
  } else {
    display_cols <- c(display_cols, "total_affected")
  }
  display_cols <- c(display_cols, rp_cols, obs_cols)
  display <- display |> select(all_of(c(display_cols, trig_cols)))

  # Helpers
  max_affected <- max(display$total_affected, na.rm = TRUE)
  if (is.infinite(max_affected) || is.na(max_affected)) max_affected <- 1
  max_bar <- max_affected
  if (has_interp) {
    max_bar <- max(max_bar,
                   max(display$total_affected_interp, na.rm = TRUE),
                   na.rm = TRUE)
  }

  fmt_compact <- function(v) {
    if (v >= 1e6) sprintf("%.1fM", v / 1e6)
    else if (v >= 1e3) sprintf("%.0fK", v / 1e3)
    else as.character(as.integer(v))
  }

  make_bar_transform <- function(color, max_val) {
    function(x) {
      vals <- suppressWarnings(as.numeric(x))
      lapply(vals, function(v) {
        if (is.na(v)) return(html(""))
        pct <- max(v / max_val * 100, 3)
        lbl <- fmt_compact(v)
        txt_col <- if (pct > 25) "white" else "#333"
        html(sprintf(
          paste0(
            '<div style="position:relative;width:100%%;height:16px;">',
            '<div style="background:%s;height:100%%;width:%.0f%%;',
            'border-radius:2px;"></div>',
            '<span style="position:absolute;left:4px;top:0;',
            'font-size:10px;line-height:16px;font-weight:bold;',
            'color:%s;white-space:nowrap;">%s</span></div>'
          ),
          color, pct, txt_col, lbl
        ))
      })
    }
  }

  # Build GT
  tbl <- display |>
    gt() |>
    cols_hide(columns = all_of(trig_cols)) |>
    cols_label(
      year = "Year",
      hit_priority = "P",
      cerf_label = "CERF",
      total_affected = "Reported",
      obs_rp_primera = "Pri",
      obs_rp_postrera = "Post"
    ) |>
    fmt_number(columns = all_of(c(rp_cols, obs_cols)), decimals = 1) |>
    sub_missing(columns = c(hit_priority, all_of(rp_cols), all_of(obs_cols)),
                missing_text = "")

  # Label LT columns with threshold RP in header
  for (col in active_lts_ordered) {
    rp_col_name <- paste0("rp_", col)
    threshold_val <- config_row[[col]]
    label <- sprintf("%s\nRP %.1f", lt_labels[[col]], threshold_val)
    tbl <- tbl |> cols_label(!!rp_col_name := label)
  }

  # Spanners
  p_rp_cols <- paste0("rp_", p_lts_ordered)
  s_rp_cols <- paste0("rp_", s_lts_ordered)

  p_spanner <- "Primera Forecast RP"
  s_spanner <- "Postrera Forecast RP"
  if ("p_seasonal_rp" %in% names(config_row) && !is.na(config_row$p_seasonal_rp)) {
    p_spanner <- sprintf("Primera Forecast RP (%.1f)", config_row$p_seasonal_rp)
  }
  if ("s_seasonal_rp" %in% names(config_row) && !is.na(config_row$s_seasonal_rp)) {
    s_spanner <- sprintf("Postrera Forecast RP (%.1f)", config_row$s_seasonal_rp)
  }

  tbl <- tbl |>
    tab_spanner(label = p_spanner, columns = all_of(p_rp_cols)) |>
    tab_spanner(label = s_spanner, columns = all_of(s_rp_cols)) |>
    tab_spanner(label = "ERA5 Obs RP", columns = all_of(obs_cols))

  # EM-DAT bar
  tbl <- tbl |>
    text_transform(
      locations = cells_body(columns = total_affected),
      fn = make_bar_transform("#de2d26", max_bar)
    )

  if (has_interp) {
    tbl <- tbl |>
      cols_label(total_affected_interp = "Interpolated") |>
      text_transform(
        locations = cells_body(columns = total_affected_interp),
        fn = make_bar_transform("#e6550d", max_bar)
      ) |>
      tab_spanner(
        label = "EM-DAT Affected",
        columns = c(total_affected, total_affected_interp)
      )
  }

  # Style: triggered cells green + bold
  for (col in active_lts_ordered) {
    rp_col_name <- paste0("rp_", col)
    trig_col_name <- paste0("trig_", col)
    triggered_rows <- which(display[[trig_col_name]])
    if (length(triggered_rows) > 0) {
      tbl <- tbl |>
        tab_style(
          style = list(cell_fill(color = "#d4edda"), cell_text(weight = "bold")),
          locations = cells_body(columns = !!rp_col_name, rows = triggered_rows)
        )
    }
  }

  # Style: CERF green
  is_cerf <- which(display$cerf_label == "\u2713")
  if (length(is_cerf) > 0) {
    tbl <- tbl |>
      tab_style(
        style = cell_fill(color = "#d4edda"),
        locations = cells_body(columns = cerf_label, rows = is_cerf)
      )
  }

  # Style: priority coloring
  is_p1 <- which(!is.na(display$hit_priority) & display$hit_priority == 1)
  is_p2 <- which(!is.na(display$hit_priority) & display$hit_priority == 2)
  if (length(is_p1) > 0) {
    tbl <- tbl |>
      tab_style(style = cell_fill(color = "#fde0dd"),
                locations = cells_body(columns = hit_priority, rows = is_p1))
  }
  if (length(is_p2) > 0) {
    tbl <- tbl |>
      tab_style(style = cell_fill(color = "#fff7bc"),
                locations = cells_body(columns = hit_priority, rows = is_p2))
  }

  # Style: obs RP brown gradient
  max_rp_val <- max(c(display$obs_rp_primera, display$obs_rp_postrera), na.rm = TRUE)
  if (is.infinite(max_rp_val) || is.na(max_rp_val)) max_rp_val <- n_years

  tbl <- tbl |>
    data_color(
      columns = all_of(obs_cols),
      palette = c("#FDEBD0", "#8B4513"),
      domain = c(1, max_rp_val),
      na_color = "white"
    )

  # Title
  annual_rp_str <- if ("annual_rp" %in% names(config_row)) {
    sprintf(" | Annual RP = %.2f", config_row$annual_rp)
  } else ""

  tbl <- tbl |>
    tab_header(
      title = sprintf("Trigger Activation: %s (%d\u2013%d)",
                      iso3_code, eval_start, eval_end),
      subtitle = sprintf("Green = trigger fires at that leadtime%s", annual_rp_str)
    ) |>
    cols_align(align = "center", columns = -year) |>
    cols_width(
      total_affected ~ px(130),
      any_of("total_affected_interp") ~ px(130)
    ) |>
    tab_options(table.font.size = px(11), data_row.padding = px(2))

  tbl
}
