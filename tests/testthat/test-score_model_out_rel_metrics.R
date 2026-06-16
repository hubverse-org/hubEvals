test_that("score_model_out succeeds with valid inputs: quantile output_type, relative wis and ae, no baseline", {
  skip_if_not_installed("hubExamples")
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  # Wilcoxon test warns about ties/zeroes on small samples; expected, not a bug
  act_scores <- suppressWarnings(score_model_out(
    model_out_tbl = forecast_outputs |>
      dplyr::filter(.data[["output_type"]] == "quantile"),
    oracle_output = forecast_oracle_output,
    metrics = c(
      "ae_median",
      "wis",
      "interval_coverage_80",
      "interval_coverage_90"
    ),
    relative_metrics = c("ae_median", "wis"),
    by = c("model_id", "location")
  ))

  exp_scores <- read.csv(test_path("testdata", "exp_pairwise_scores.csv")) |>
    dplyr::mutate(location = as.character(location)) |>
    dplyr::select(-ae_median_scaled_relative_skill, -wis_scaled_relative_skill)

  expect_equal(act_scores, exp_scores, ignore_attr = TRUE)
})


test_that("score_model_out succeeds with valid inputs: quantile output_type, relative wis and ae, Flusight-baseline", {
  skip_if_not_installed("hubExamples")
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  # Wilcoxon test warns about ties/zeroes on small samples; expected, not a bug
  act_scores <- suppressWarnings(score_model_out(
    model_out_tbl = forecast_outputs |>
      dplyr::filter(.data[["output_type"]] == "quantile"),
    oracle_output = forecast_oracle_output,
    metrics = c(
      "ae_median",
      "wis",
      "interval_coverage_80",
      "interval_coverage_90"
    ),
    relative_metrics = c("ae_median", "wis"),
    baseline = "Flusight-baseline",
    by = c("model_id", "location")
  ))

  exp_scores <- read.csv(test_path("testdata", "exp_pairwise_scores.csv")) |>
    dplyr::mutate(location = as.character(location))

  expect_equal(act_scores, exp_scores, ignore_attr = TRUE)
})


test_that("score_model_out errors when invalid relative metrics are requested", {
  skip_if_not_installed("hubExamples")
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  # not allowed to compute relative skill for interval coverage
  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |>
        dplyr::filter(.data[["output_type"]] == "quantile"),
      oracle_output = forecast_oracle_output,
      metrics = c("wis", "interval_coverage_80", "interval_coverage_90"),
      relative_metrics = c("interval_coverage_90", "wis"),
    ),
    regexp = "Interval coverage metrics are not supported for relative skill scores."
  )

  # not allowed to compute relative skill for bias: hubEvals errors at the
  # boundary, before scoringutils fails with its "same sign" message.
  quantile_out <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "quantile")
  expect_error(
    score_model_out(
      model_out_tbl = quantile_out,
      oracle_output = forecast_oracle_output,
      metrics = c("wis", "bias"),
      relative_metrics = "bias"
    ),
    regexp = "bias.*not supported for relative skill scores"
  )

  # relative_metrics must be a subset of metrics
  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |>
        dplyr::filter(.data[["output_type"]] == "quantile"),
      oracle_output = forecast_oracle_output,
      metrics = c("wis", "interval_coverage_80", "interval_coverage_90"),
      relative_metrics = c("ae_median", "wis"),
    ),
    regexp = "Relative metrics must be a subset of the metrics."
  )

  # can't ask for relative metrics without breaking down by model_id
  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |>
        dplyr::filter(.data[["output_type"]] == "quantile"),
      oracle_output = forecast_oracle_output,
      metrics = c("wis", "interval_coverage_80", "interval_coverage_90"),
      relative_metrics = "wis",
      by = "location"
    ),
    regexp = "Relative metrics require 'model_id' to be included in `by`."
  )
})


test_that("score_model_out handles single-model input gracefully for relative metrics", {
  skip_if_not_installed("hubExamples")
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  single <- forecast_outputs |>
    dplyr::filter(
      .data[["output_type"]] == "quantile",
      .data[["model_id"]] == "Flusight-baseline"
    )

  # No baseline: just fills in the relative-skill column with 1
  scores_no_baseline <- score_model_out(
    model_out_tbl = single,
    oracle_output = forecast_oracle_output,
    metrics = c("wis", "ae_median"),
    relative_metrics = c("wis", "ae_median")
  )
  expect_equal(nrow(scores_no_baseline), 1L)
  expect_equal(scores_no_baseline$wis_relative_skill, 1)
  expect_equal(scores_no_baseline$ae_median_relative_skill, 1)
  expect_false("wis_scaled_relative_skill" %in% names(scores_no_baseline))
  expect_true(all(
    c("wis_relative_skill", "ae_median_relative_skill") %in%
      attr(scores_no_baseline, "metrics")
  ))

  # Baseline matching the lone model: both relative_skill and scaled_relative_skill = 1
  scores_baseline <- score_model_out(
    model_out_tbl = single,
    oracle_output = forecast_oracle_output,
    metrics = "wis",
    relative_metrics = "wis",
    baseline = "Flusight-baseline"
  )
  expect_equal(scores_baseline$wis_relative_skill, 1)
  expect_equal(scores_baseline$wis_scaled_relative_skill, 1)
  expect_true(all(
    c("wis_relative_skill", "wis_scaled_relative_skill") %in%
      attr(scores_baseline, "metrics")
  ))

  # Baseline that is not the lone model: errors
  expect_error(
    score_model_out(
      model_out_tbl = single,
      oracle_output = forecast_oracle_output,
      metrics = "wis",
      relative_metrics = "wis",
      baseline = "MOBS-GLEAM_FLUH"
    ),
    regexp = "baseline.*MOBS-GLEAM_FLUH.*not present"
  )
})
