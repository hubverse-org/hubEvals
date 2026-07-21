test_that("score_model_out succeeds with valid inputs: quantile output_type, relative wis and ae, no baseline", {
  skip_if_not_installed("hubExamples")
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  act_scores <- score_model_out(
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
  )

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

  act_scores <- score_model_out(
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
  )

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


test_that("score_model_out reports NA relative skill and warns when a baseline is missing from a group", {
  skip_if_not_installed("hubExamples")
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  quantile_out <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "quantile")

  # Drop the baseline's predictions for one location entirely, so the location
  # 25 comparison group (defined by `by`) contains no baseline.
  is_dropped <- quantile_out[["model_id"]] == "Flusight-baseline" &
    quantile_out[["location"]] == "25"
  incomplete <- quantile_out[!is_dropped, ]

  # Absolute scores computed without relative metrics, as the reference point
  # for "absolute scores and counts unchanged".
  base_scores <- score_model_out(
    model_out_tbl = incomplete,
    oracle_output = forecast_oracle_output,
    metrics = c("wis", "ae_median"),
    by = c("model_id", "location")
  )

  expect_warning(
    rel_scores <- score_model_out(
      model_out_tbl = incomplete,
      oracle_output = forecast_oracle_output,
      metrics = c("wis", "ae_median"),
      relative_metrics = "wis",
      baseline = "Flusight-baseline",
      by = c("model_id", "location")
    ),
    regexp = "Flusight-baseline.*location"
  )

  # Relative skill is NA exactly for the baseline-absent group (location 25)
  # and finite for the group the baseline covers (location 48).
  loc25 <- rel_scores[rel_scores[["location"]] == "25", ]
  loc48 <- rel_scores[rel_scores[["location"]] == "48", ]
  expect_true(all(is.na(loc25[["wis_relative_skill"]])))
  expect_true(all(is.na(loc25[["wis_scaled_relative_skill"]])))
  expect_true(all(is.finite(loc48[["wis_relative_skill"]])))
  expect_true(all(is.finite(loc48[["wis_scaled_relative_skill"]])))

  # Absolute scores and row counts are unchanged by requesting relative metrics.
  expect_equal(nrow(rel_scores), nrow(base_scores))
  key <- c("model_id", "location")
  joined <- dplyr::left_join(
    base_scores[, c(key, "wis", "ae_median")],
    rel_scores[, c(key, "wis", "ae_median")],
    by = key,
    suffix = c("_base", "_rel")
  )
  expect_equal(joined[["wis_base"]], joined[["wis_rel"]])
  expect_equal(joined[["ae_median_base"]], joined[["ae_median_rel"]])
})


test_that("score_model_out still computes relative skill when a baseline has partial (within-group) coverage", {
  skip_if_not_installed("hubExamples")
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  quantile_out <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "quantile")

  # Drop only some of the baseline's location 25 predictions (one reference
  # date), so the baseline is still present in the location 25 group. Within a
  # group scoringutils handles partial overlap, so this must still compute.
  is_dropped <- quantile_out[["model_id"]] == "Flusight-baseline" &
    quantile_out[["location"]] == "25" &
    quantile_out[["reference_date"]] == "2022-11-19"
  partial <- quantile_out[!is_dropped, ]

  # No baseline-coverage warning, and relative skill computed for every group.
  scores <- expect_no_warning(
    score_model_out(
      model_out_tbl = partial,
      oracle_output = forecast_oracle_output,
      metrics = "wis",
      relative_metrics = "wis",
      baseline = "Flusight-baseline",
      by = c("model_id", "location")
    ),
    message = "could not be scaled"
  )
  expect_true(all(is.finite(scores[["wis_relative_skill"]])))
})


test_that("score_model_out errors when the baseline is absent from the data entirely", {
  skip_if_not_installed("hubExamples")
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  quantile_out <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "quantile")

  # A baseline name that appears nowhere in the data is a user error, not an
  # incomplete-coverage case, and gets a clear hubEvals message rather than
  # scoringutils' checkmate assertion.
  expect_error(
    score_model_out(
      model_out_tbl = quantile_out,
      oracle_output = forecast_oracle_output,
      metrics = "wis",
      relative_metrics = "wis",
      baseline = "not-a-real-model",
      by = c("model_id", "location")
    ),
    regexp = "baseline.*not-a-real-model.*not present"
  )
})


test_that("score_model_out computes relative skill without a coverage warning when the baseline covers every group", {
  skip_if_not_installed("hubExamples")
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  quantile_out <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "quantile")

  # Assert only that the baseline-coverage warning is absent.
  expect_no_warning(
    score_model_out(
      model_out_tbl = quantile_out,
      oracle_output = forecast_oracle_output,
      metrics = c("wis", "ae_median"),
      relative_metrics = "wis",
      baseline = "Flusight-baseline",
      by = c("model_id", "location")
    ),
    message = "could not be scaled"
  )
})


test_that("score_model_out computes relative skill with no disaggregation when the baseline has partial coverage", {
  skip_if_not_installed("hubExamples")
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  quantile_out <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "quantile")

  # With `by = "model_id"` there is a single comparison group spanning the whole
  # data, so a baseline that is missing an entire location is still present in
  # that one group. scoringutils compares each pair on its overlapping forecast
  # units, so relative skill is computed (no NA, no coverage warning).
  is_dropped <- quantile_out[["model_id"]] == "Flusight-baseline" &
    quantile_out[["location"]] == "25"
  partial <- quantile_out[!is_dropped, ]

  # Assert only that the baseline-coverage warning is absent.
  scores <- expect_no_warning(
    score_model_out(
      model_out_tbl = partial,
      oracle_output = forecast_oracle_output,
      metrics = "wis",
      relative_metrics = "wis",
      baseline = "Flusight-baseline",
      by = "model_id"
    ),
    message = "could not be scaled"
  )

  # One summarised row per model, all relative skill finite.
  expect_equal(nrow(scores), 3L)
  expect_true(all(is.finite(scores[["wis_relative_skill"]])))
  expect_true(all(is.finite(scores[["wis_scaled_relative_skill"]])))
})


test_that("score_model_out fills relative skill with 1 for single-model disaggregation groups", {
  skip_if_not_installed("hubExamples")
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  quantile_out <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "quantile")

  # Make location 25 a single-model group (only MOBS-GLEAM_FLUH); location 48
  # keeps all models. A lone-model group has no comparator, so -- consistent
  # with the global single-model case -- its relative skill is 1, not NA, and
  # it does not warn.
  drop_others <- quantile_out[["location"]] == "25" &
    quantile_out[["model_id"]] != "MOBS-GLEAM_FLUH"
  one_model_loc <- quantile_out[!drop_others, ]

  scores <- expect_no_warning(
    score_model_out(
      model_out_tbl = one_model_loc,
      oracle_output = forecast_oracle_output,
      metrics = "wis",
      relative_metrics = "wis",
      by = c("model_id", "location")
    ),
    message = "could not be scaled"
  )

  loc25 <- scores[scores[["location"]] == "25", ]
  loc48 <- scores[scores[["location"]] == "48", ]
  expect_equal(loc25[["wis_relative_skill"]], 1)
  expect_true(all(is.finite(loc48[["wis_relative_skill"]])))
})


test_that("score_model_out fills 1 for a single-model group made up of the baseline", {
  skip_if_not_installed("hubExamples")
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  quantile_out <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "quantile")

  # Location 25 contains only the baseline model. The baseline is present in
  # that group (so it is not skipped to NA) but has no comparator, so relative
  # and scaled relative skill are both 1, with no warning.
  drop_others <- quantile_out[["location"]] == "25" &
    quantile_out[["model_id"]] != "Flusight-baseline"
  baseline_only_loc <- quantile_out[!drop_others, ]

  scores <- expect_no_warning(
    score_model_out(
      model_out_tbl = baseline_only_loc,
      oracle_output = forecast_oracle_output,
      metrics = "wis",
      relative_metrics = "wis",
      baseline = "Flusight-baseline",
      by = c("model_id", "location")
    ),
    message = "could not be scaled"
  )

  loc25 <- scores[scores[["location"]] == "25", ]
  expect_equal(loc25[["wis_relative_skill"]], 1)
  expect_equal(loc25[["wis_scaled_relative_skill"]], 1)
})
