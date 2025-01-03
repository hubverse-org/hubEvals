#' Geometric mean
#' (x_1 \times x_2 \times \ldots \times x_n)^{1/n}
#'  = exp[1/n \sum_{i=1}^{n} log(x_i)]
geometric_mean <- function(x) {
  exp(mean(log(x)))
}


#' Helper function manually computes pairwise relative skill scores
#' by location
get_pairwise_scores_by_loc <- function(scores_per_task, metric, baseline = NULL) {
  mean_scores_by_loc <- scores_per_task |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("model_id", "location")))) |>
    dplyr::summarize(
      mean_score = mean(.data[[metric]], na.rm = TRUE),
      .groups = "drop"
    )

  pairwise_score_ratios <- tidyr::expand_grid(
    model_id = unique(mean_scores_by_loc$model_id),
    model_id_compare = unique(mean_scores_by_loc$model_id),
    location = unique(mean_scores_by_loc[["location"]])
  ) |>
    dplyr::left_join(mean_scores_by_loc, by = c("model_id" = "model_id", "location")) |>
    dplyr::left_join(mean_scores_by_loc, by = c("model_id_compare" = "model_id", "location")) |>
    dplyr::mutate(
      pairwise_score_ratio = mean_score.x / mean_score.y # nolint: object_usage_linter
    )

  result <- pairwise_score_ratios |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("model_id", "location")))) |>
    dplyr::summarize(
      relative_skill = geometric_mean(pairwise_score_ratio), # nolint: object_usage_linter
      .groups = "drop"
    )
  out_colnames <- c("model_id", "location", paste0(metric, "_relative_skill"))

  if (!is.null(baseline)) {
    result <- result |>
      dplyr::group_by(location) |> # nolint: object_usage_linter
      dplyr::mutate(
        scaled_relative_skill = relative_skill / relative_skill[model_id == baseline] # nolint: object_usage_linter
      )
    out_colnames <- c(out_colnames, paste0(metric, "_scaled_relative_skill"))
  }

  colnames(result) <- out_colnames

  return(result)
}


test_that("score_model_out succeeds with valid inputs: quantile output_type, relative wis and ae, no baseline", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
    oracle_output = forecast_oracle_output,
    metrics = c("ae_median", "wis", "interval_coverage_80", "interval_coverage_90"),
    relative_metrics = c("ae_median", "wis"),
    by = c("model_id", "location")
  )

  exp_scores_unsummarized <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "quantile") |>
    dplyr::left_join(
      forecast_oracle_output |>
        dplyr::filter(.data[["output_type"]] == "quantile") |>
        dplyr::select(-dplyr::all_of(c("output_type", "output_type_id"))),
      by = c("location", "target_end_date", "target")
    ) |>
    dplyr::mutate(
      output_type_id = as.numeric(.data[["output_type_id"]]),
      qs = ifelse(
        .data[["oracle_value"]] >= .data[["value"]],
        .data[["output_type_id"]] * (.data[["oracle_value"]] - .data[["value"]]),
        (1 - .data[["output_type_id"]]) * (.data[["value"]] - .data[["oracle_value"]])
      ),
      q_coverage_80_lower = ifelse(
        .data[["output_type_id"]] == 0.1,
        .data[["oracle_value"]] >= .data[["value"]],
        NA_real_
      ),
      q_coverage_80_upper = ifelse(
        .data[["output_type_id"]] == 0.9,
        .data[["oracle_value"]] <= .data[["value"]],
        NA_real_
      ),
      q_coverage_90_lower = ifelse(
        .data[["output_type_id"]] == 0.05,
        .data[["oracle_value"]] >= .data[["value"]],
        NA_real_
      ),
      q_coverage_90_upper = ifelse(
        .data[["output_type_id"]] == 0.95,
        .data[["oracle_value"]] <= .data[["value"]],
        NA_real_
      )
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("model_id", "location", "reference_date", "horizon", "target_end_date", "target")
    ))) |>
    dplyr::summarize(
      ae_median = sum(ifelse(
        .data[["output_type_id"]] == 0.5,
        abs(.data[["oracle_value"]] - .data[["value"]]),
        0
      )),
      wis = 2 * mean(.data[["qs"]]),
      interval_coverage_80 = (sum(.data[["q_coverage_80_lower"]], na.rm = TRUE) == 1) *
        (sum(.data[["q_coverage_80_upper"]], na.rm = TRUE) == 1),
      interval_coverage_90 = (sum(.data[["q_coverage_90_lower"]], na.rm = TRUE) == 1) *
        (sum(.data[["q_coverage_90_upper"]], na.rm = TRUE) == 1)
    )

  exp_scores_standard <- exp_scores_unsummarized |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("model_id", "location")
    ))) |>
    dplyr::summarize(
      ae_median = mean(.data[["ae_median"]]),
      wis = mean(.data[["wis"]]),
      interval_coverage_80 = mean(.data[["interval_coverage_80"]], na.rm = TRUE),
      interval_coverage_90 = mean(.data[["interval_coverage_90"]], na.rm = TRUE),
      .groups = "drop"
    )

  exp_scores_relative_ae_median <- get_pairwise_scores_by_loc(exp_scores_unsummarized, "ae_median")
  exp_scores_relative_wis <- get_pairwise_scores_by_loc(exp_scores_unsummarized, "wis")
  exp_scores <- exp_scores_standard |>
    dplyr::full_join(exp_scores_relative_ae_median, by = c("model_id", "location")) |>
    dplyr::full_join(exp_scores_relative_wis, by = c("model_id", "location"))

  # same column names, number of rows, and score values
  expect_equal(colnames(act_scores), colnames(exp_scores))
  expect_equal(nrow(act_scores), nrow(exp_scores))
  merged_scores <- dplyr::full_join(
    act_scores, exp_scores,
    by = c("model_id", "location")
  )
  expect_equal(nrow(act_scores), nrow(merged_scores))
  expect_equal(merged_scores$ae_median.x, merged_scores$ae_median.y)
  expect_equal(merged_scores$wis.x, merged_scores$wis.y)
  expect_equal(merged_scores$interval_coverage_80.x, merged_scores$interval_coverage_80.y)
  expect_equal(merged_scores$interval_coverage_90.x, merged_scores$interval_coverage_90.y)
  expect_equal(merged_scores$ae_median_relative_skill.x, merged_scores$ae_median_relative_skill.y)
  expect_equal(merged_scores$wis_relative_skill.x, merged_scores$wis_relative_skill.y)
})


test_that("score_model_out succeeds with valid inputs: quantile output_type, relative wis and ae, Flusight-baseline", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
    oracle_output = forecast_oracle_output,
    metrics = c("ae_median", "wis", "interval_coverage_80", "interval_coverage_90"),
    relative_metrics = c("ae_median", "wis"),
    baseline = "Flusight-baseline",
    by = c("model_id", "location")
  )

  exp_scores_unsummarized <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "quantile") |>
    dplyr::left_join(
      forecast_oracle_output |>
        dplyr::filter(.data[["output_type"]] == "quantile") |>
        dplyr::select(-dplyr::all_of(c("output_type", "output_type_id"))),
      by = c("location", "target_end_date", "target")
    ) |>
    dplyr::mutate(
      output_type_id = as.numeric(.data[["output_type_id"]]),
      qs = ifelse(
        .data[["oracle_value"]] >= .data[["value"]],
        .data[["output_type_id"]] * (.data[["oracle_value"]] - .data[["value"]]),
        (1 - .data[["output_type_id"]]) * (.data[["value"]] - .data[["oracle_value"]])
      ),
      q_coverage_80_lower = ifelse(
        .data[["output_type_id"]] == 0.1,
        .data[["oracle_value"]] >= .data[["value"]],
        NA_real_
      ),
      q_coverage_80_upper = ifelse(
        .data[["output_type_id"]] == 0.9,
        .data[["oracle_value"]] <= .data[["value"]],
        NA_real_
      ),
      q_coverage_90_lower = ifelse(
        .data[["output_type_id"]] == 0.05,
        .data[["oracle_value"]] >= .data[["value"]],
        NA_real_
      ),
      q_coverage_90_upper = ifelse(
        .data[["output_type_id"]] == 0.95,
        .data[["oracle_value"]] <= .data[["value"]],
        NA_real_
      )
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("model_id", "location", "reference_date", "horizon", "target_end_date", "target")
    ))) |>
    dplyr::summarize(
      ae_median = sum(ifelse(
        .data[["output_type_id"]] == 0.5,
        abs(.data[["oracle_value"]] - .data[["value"]]),
        0
      )),
      wis = 2 * mean(.data[["qs"]]),
      interval_coverage_80 = (sum(.data[["q_coverage_80_lower"]], na.rm = TRUE) == 1) *
        (sum(.data[["q_coverage_80_upper"]], na.rm = TRUE) == 1),
      interval_coverage_90 = (sum(.data[["q_coverage_90_lower"]], na.rm = TRUE) == 1) *
        (sum(.data[["q_coverage_90_upper"]], na.rm = TRUE) == 1)
    )

  exp_scores_standard <- exp_scores_unsummarized |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("model_id", "location")
    ))) |>
    dplyr::summarize(
      ae_median = mean(.data[["ae_median"]]),
      wis = mean(.data[["wis"]]),
      interval_coverage_80 = mean(.data[["interval_coverage_80"]], na.rm = TRUE),
      interval_coverage_90 = mean(.data[["interval_coverage_90"]], na.rm = TRUE),
      .groups = "drop"
    )

  exp_scores_relative_ae_median <- get_pairwise_scores_by_loc(exp_scores_unsummarized, "ae_median", "Flusight-baseline")
  exp_scores_relative_wis <- get_pairwise_scores_by_loc(exp_scores_unsummarized, "wis", "Flusight-baseline")
  exp_scores <- exp_scores_standard |>
    dplyr::full_join(exp_scores_relative_ae_median, by = c("model_id", "location")) |>
    dplyr::full_join(exp_scores_relative_wis, by = c("model_id", "location"))

  # same column names, number of rows, and score values
  expect_equal(colnames(act_scores), colnames(exp_scores))
  expect_equal(nrow(act_scores), nrow(exp_scores))
  merged_scores <- dplyr::full_join(
    act_scores, exp_scores,
    by = c("model_id", "location")
  )
  expect_equal(nrow(act_scores), nrow(merged_scores))
  expect_equal(merged_scores$ae_median.x, merged_scores$ae_median.y)
  expect_equal(merged_scores$wis.x, merged_scores$wis.y)
  expect_equal(merged_scores$interval_coverage_80.x, merged_scores$interval_coverage_80.y)
  expect_equal(merged_scores$interval_coverage_90.x, merged_scores$interval_coverage_90.y)
  expect_equal(merged_scores$ae_median_relative_skill.x, merged_scores$ae_median_relative_skill.y)
  expect_equal(merged_scores$ae_median_scaled_relative_skill.x, merged_scores$ae_median_scaled_relative_skill.y)
  expect_equal(merged_scores$wis_relative_skill.x, merged_scores$wis_relative_skill.y)
  expect_equal(merged_scores$wis_scaled_relative_skill.x, merged_scores$wis_scaled_relative_skill.y)
})


test_that("score_model_out errors when invalid relative metrics are requested", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  # not allowed to compute relative skill for interval coverage
  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
      oracle_output = forecast_oracle_output,
      metrics = c("wis", "interval_coverage_80", "interval_coverage_90"),
      relative_metrics = c("interval_coverage_90", "wis"),
    ),
    regexp = "Interval coverage metrics are not supported for relative skill scores."
  )

  # relative_metrics must be a subset of metrics
  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
      oracle_output = forecast_oracle_output,
      metrics = c("wis", "interval_coverage_80", "interval_coverage_90"),
      relative_metrics = c("ae_median", "wis"),
    ),
    regexp = "Relative metrics must be a subset of the metrics."
  )

  # can't ask for relative metrics without breaking down by model_id
  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
      oracle_output = forecast_oracle_output,
      metrics = c("wis", "interval_coverage_80", "interval_coverage_90"),
      relative_metrics = "wis",
      by = "location"
    ),
    regexp = "Relative metrics require 'model_id' to be included in `by`."
  )
})
