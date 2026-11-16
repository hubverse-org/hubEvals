# Run `expr`, returning its value (or error message) and any warnings, so two
# calls can be compared on everything they do.
capture_outcome <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) {
      structure(conditionMessage(e), class = "error_message")
    }),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = warnings)
}

quantile_outputs <- function() {
  hubExamples::forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "quantile")
}


test_that("summarize_model_out_scores matches score_model_out for quantile forecasts", {
  skip_if_not_installed("hubExamples")
  model_out_tbl <- quantile_outputs()
  oracle_output <- hubExamples::forecast_oracle_output
  metrics <- c(
    "wis",
    "ae_median",
    "interval_coverage_50",
    "interval_coverage_90"
  )
  log_shift <- list(
    transform = scoringutils::log_shift,
    transform_label = "log",
    offset = 1
  )

  # "scale" is in `by` when transformed scores are appended, as
  # hubPredEvalsData passes it, so relative skill keeps the two scales apart
  cases <- list(
    list(
      transform = list(),
      by = "model_id",
      relative_metrics = "wis",
      baseline = "Flusight-baseline",
      include_count = TRUE
    ),
    list(
      transform = list(),
      by = c("model_id", "reference_date"),
      relative_metrics = NULL,
      baseline = NULL,
      include_count = FALSE
    ),
    list(
      transform = c(log_shift, transform_append = TRUE),
      by = c("model_id", "location", "scale"),
      relative_metrics = c("wis", "ae_median"),
      baseline = NULL,
      include_count = FALSE
    ),
    list(
      transform = c(log_shift, transform_append = FALSE),
      by = c("model_id", "horizon"),
      relative_metrics = "wis",
      baseline = "Flusight-baseline",
      include_count = TRUE
    )
  )
  for (case in cases) {
    args <- list(
      metrics = metrics,
      relative_metrics = case$relative_metrics,
      baseline = case$baseline,
      by = case$by,
      include_count = case$include_count
    )
    expected <- capture_outcome(
      do.call(
        score_model_out,
        c(list(model_out_tbl, oracle_output), args, case$transform)
      )
    )
    row_scores <- do.call(
      score_model_out,
      c(
        list(
          model_out_tbl,
          oracle_output,
          metrics = metrics,
          summarize = FALSE
        ),
        case$transform
      )
    )
    actual <- capture_outcome(do.call(
      summarize_model_out_scores,
      c(list(row_scores), args[-1])
    ))
    expect_identical(actual, expected, label = toString(case$by))
  }
})


test_that("summarizing a subset of the scores matches scoring that subset", {
  skip_if_not_installed("hubExamples")
  model_out_tbl <- quantile_outputs()
  oracle_output <- hubExamples::forecast_oracle_output
  metrics <- c("wis", "ae_median", "interval_coverage_90")
  row_scores <- score_model_out(
    model_out_tbl,
    oracle_output,
    metrics = metrics,
    summarize = FALSE,
    transform = scoringutils::log_shift,
    transform_append = TRUE,
    transform_label = "log",
    offset = 1
  )

  locations <- unique(model_out_tbl$location)[1:2]
  ref_date <- max(model_out_tbl$reference_date)
  keep <- function(df) {
    df$location %in% locations & df$reference_date == ref_date & df$horizon <= 1
  }
  subset_scores <- row_scores[keep(row_scores), ]
  attr(subset_scores, "metrics") <- attr(row_scores, "metrics")

  by <- c("model_id", "location", "scale")
  expected <- capture_outcome(score_model_out(
    model_out_tbl[keep(model_out_tbl), ],
    oracle_output,
    metrics = metrics,
    relative_metrics = "wis",
    baseline = "Flusight-baseline",
    by = by,
    include_count = TRUE,
    transform = scoringutils::log_shift,
    transform_append = TRUE,
    transform_label = "log",
    offset = 1
  ))
  actual <- capture_outcome(summarize_model_out_scores(
    subset_scores,
    relative_metrics = "wis",
    baseline = "Flusight-baseline",
    by = by,
    include_count = TRUE
  ))
  expect_identical(actual, expected)
})


test_that("summarize_model_out_scores matches score_model_out for other output types", {
  skip_if_not_installed("hubExamples")
  forecast_outputs <- hubExamples::forecast_outputs
  oracle_output <- hubExamples::forecast_oracle_output
  cases <- list(
    list(type = "median", metrics = "ae_point", relative = "ae_point"),
    list(type = "mean", metrics = "se_point", relative = "se_point"),
    list(type = "pmf", metrics = "log_score", relative = NULL),
    list(type = "sample", metrics = "crps", relative = "crps")
  )
  for (case in cases) {
    model_out_tbl <- dplyr::filter(
      forecast_outputs,
      .data[["output_type"]] == case$type
    )
    by <- c("model_id", "location")
    expected <- capture_outcome(score_model_out(
      model_out_tbl,
      oracle_output,
      metrics = case$metrics,
      relative_metrics = case$relative,
      by = by
    ))
    row_scores <- score_model_out(
      model_out_tbl,
      oracle_output,
      metrics = case$metrics,
      summarize = FALSE
    )
    actual <- capture_outcome(summarize_model_out_scores(
      row_scores,
      relative_metrics = case$relative,
      by = by
    ))
    expect_identical(actual, expected, label = case$type)
  }
})


test_that("summarize_model_out_scores matches score_model_out in relative-skill edge cases", {
  skip_if_not_installed("hubExamples")
  model_out_tbl <- quantile_outputs()
  oracle_output <- hubExamples::forecast_oracle_output
  metrics <- c("wis", "ae_median")
  first_location <- unique(model_out_tbl$location)[1]

  cases <- list(
    # a single model: relative skill of 1
    single_model = dplyr::filter(
      model_out_tbl,
      .data[["model_id"]] == "Flusight-baseline"
    ),
    # baseline missing from one location: NA there, with a warning
    baseline_gap = dplyr::filter(
      model_out_tbl,
      .data[["model_id"]] != "Flusight-baseline" |
        .data[["location"]] != first_location
    ),
    # one location forecast by a single model: a trivial group
    single_model_group = dplyr::filter(
      model_out_tbl,
      .data[["model_id"]] == "Flusight-baseline" |
        .data[["location"]] != first_location
    )
  )
  for (name in names(cases)) {
    by <- c("model_id", "location")
    expected <- capture_outcome(score_model_out(
      cases[[name]],
      oracle_output,
      metrics = metrics,
      relative_metrics = metrics,
      baseline = "Flusight-baseline",
      by = by
    ))
    row_scores <- score_model_out(
      cases[[name]],
      oracle_output,
      metrics = metrics,
      summarize = FALSE
    )
    actual <- capture_outcome(summarize_model_out_scores(
      row_scores,
      relative_metrics = metrics,
      baseline = "Flusight-baseline",
      by = by
    ))
    expect_identical(actual, expected, label = name)
  }

  # a baseline absent from the data entirely is an error in both
  row_scores <- score_model_out(
    model_out_tbl,
    oracle_output,
    metrics = metrics,
    summarize = FALSE
  )
  expect_error(
    summarize_model_out_scores(
      row_scores,
      relative_metrics = "wis",
      baseline = "not-a-model"
    ),
    "not present in the data"
  )
})


test_that("summarize_model_out_scores validates its inputs", {
  skip_if_not_installed("hubExamples")
  row_scores <- score_model_out(
    quantile_outputs(),
    hubExamples::forecast_oracle_output,
    metrics = c("wis", "interval_coverage_90"),
    summarize = FALSE
  )
  expect_error(
    summarize_model_out_scores(as.data.frame(unclass(row_scores))),
    "must be per-forecast scores"
  )
  expect_error(
    summarize_model_out_scores(row_scores, relative_metrics = "ae_median"),
    "must be a subset of the metrics"
  )
  expect_error(
    summarize_model_out_scores(
      row_scores,
      relative_metrics = "wis",
      by = "location"
    ),
    "require 'model_id'"
  )
  expect_error(
    summarize_model_out_scores(
      row_scores,
      relative_metrics = "interval_coverage_90"
    ),
    "not supported for relative skill"
  )
})


test_that("compute_skills matches scoringutils::add_relative_skill", {
  skip_if_not_installed("hubExamples")
  model_out_tbl <- quantile_outputs()
  oracle_output <- hubExamples::forecast_oracle_output
  scores <- scoringutils::score(
    transform_quantile_model_out(model_out_tbl, oracle_output),
    get_metrics(
      transform_quantile_model_out(model_out_tbl, oracle_output),
      output_type = "quantile",
      select = c("wis", "ae_median")
    )
  )
  scores <- dplyr::rename(scores, model_id = "model")
  task_id_cols <- get_task_id_cols(model_out_tbl)

  # The implementation compute_skills() replaced: add_relative_skill() per
  # metric, then one row per (model_id, group).
  via_add_relative_skill <- function(
    scores,
    group_cols,
    relative_metrics,
    baseline
  ) {
    skill_cols <- relative_skill_col_names(relative_metrics, baseline)
    for (metric in relative_metrics) {
      scores <- scoringutils::add_relative_skill(
        scores,
        compare = "model_id",
        by = group_cols,
        metric = metric,
        baseline = baseline
      )
    }
    scores |>
      tibble::as_tibble() |>
      dplyr::select(dplyr::all_of(c("model_id", group_cols, skill_cols))) |>
      dplyr::distinct()
  }

  # Compare values only: the lookup is left-joined onto the scores, which keep
  # their own `metrics` attribute, so the lookup's is never used.
  tidy <- function(x, key) {
    x <- dplyr::arrange(x, dplyr::across(dplyr::all_of(key)))
    attr(x, "metrics") <- NULL
    x
  }
  cases <- list(
    list(group_cols = character(0), relative_metrics = "wis", baseline = NULL),
    list(
      group_cols = "location",
      relative_metrics = c("wis", "ae_median"),
      baseline = "Flusight-baseline"
    ),
    # every task id: scoringutils drops `by` and compares over all forecasts
    list(
      group_cols = task_id_cols,
      relative_metrics = "wis",
      baseline = "Flusight-baseline"
    )
  )
  for (case in cases) {
    groups <- scores |>
      tibble::as_tibble() |>
      dplyr::distinct(dplyr::across(dplyr::all_of(case$group_cols)))
    skill_cols <- relative_skill_col_names(case$relative_metrics, case$baseline)
    expected <- suppressMessages(
      via_add_relative_skill(
        scores,
        case$group_cols,
        case$relative_metrics,
        case$baseline
      )
    )
    actual <- suppressMessages(
      compute_skills(
        scores,
        groups,
        case$group_cols,
        skill_cols,
        case$relative_metrics,
        case$baseline
      )
    )
    key <- c("model_id", case$group_cols)
    expect_identical(
      tidy(actual, key),
      tidy(expected, key),
      label = toString(case$group_cols)
    )
  }
})
