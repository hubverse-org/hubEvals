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

  exp_scores <- read.csv(test_path("testdata", "exp_pairwise_scores.csv")) |>
    dplyr::mutate(location = as.character(location)) |>
    dplyr::select(-ae_median_scaled_relative_skill, -wis_scaled_relative_skill)

  expect_equal(act_scores, exp_scores, ignore_attr = TRUE)
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

  exp_scores <- read.csv(test_path("testdata", "exp_pairwise_scores.csv")) |>
    dplyr::mutate(location = as.character(location))

  expect_equal(act_scores, exp_scores, ignore_attr = TRUE)
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
