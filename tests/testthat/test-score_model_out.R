test_that("score_model_out succeeds with valid inputs: mean output_type, default metrics, by all", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |>
      dplyr::filter(.data[["output_type"]] == "mean"),
    oracle_output = forecast_oracle_output,
    by = c(
      "model_id",
      "location",
      "reference_date",
      "horizon",
      "target_end_date",
      "target"
    )
  )

  exp_scores <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "mean") |>
    dplyr::left_join(
      forecast_oracle_output |>
        dplyr::filter(.data[["output_type"]] == "mean"),
      by = c("location", "target_end_date", "target")
    ) |>
    dplyr::mutate(
      se = (.data[["value"]] - .data[["oracle_value"]])^2
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c(
        "model_id",
        "location",
        "reference_date",
        "horizon",
        "target_end_date",
        "target"
      )
    ))) |>
    dplyr::summarize(
      se_point = mean(.data[["se"]]),
      .groups = "drop"
    )

  # same column names, number of rows, and score values
  expect_equal(colnames(act_scores), colnames(exp_scores))
  expect_equal(nrow(act_scores), nrow(exp_scores))
  merged_scores <- dplyr::full_join(
    act_scores,
    exp_scores,
    by = c(
      "model_id",
      "location",
      "reference_date",
      "horizon",
      "target_end_date",
      "target"
    )
  )
  expect_equal(nrow(act_scores), nrow(merged_scores))
  expect_equal(merged_scores$se_point.x, merged_scores$se_point.y)
})


test_that("score_model_out succeeds with valid inputs: mean output_type, default metrics, summarize FALSE", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |>
      dplyr::filter(.data[["output_type"]] == "mean"),
    oracle_output = forecast_oracle_output,
    summarize = FALSE
  )

  exp_scores <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "mean") |>
    dplyr::left_join(
      forecast_oracle_output |>
        dplyr::filter(.data[["output_type"]] == "mean"),
      by = c("location", "target_end_date", "target")
    ) |>
    dplyr::mutate(
      se = (.data[["value"]] - .data[["oracle_value"]])^2
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c(
        "model_id",
        "reference_date",
        "target",
        "horizon",
        "location",
        "target_end_date"
      )
    ))) |>
    dplyr::summarize(
      se_point = mean(.data[["se"]]),
      .groups = "drop"
    )

  # same column names, number of rows, and score values
  expect_equal(colnames(act_scores), colnames(exp_scores))
  expect_equal(nrow(act_scores), nrow(exp_scores))
  merged_scores <- dplyr::full_join(
    act_scores,
    exp_scores,
    by = c(
      "model_id",
      "location",
      "reference_date",
      "horizon",
      "target_end_date",
      "target"
    )
  )
  expect_equal(nrow(act_scores), nrow(merged_scores))
  expect_equal(merged_scores$se_point.x, merged_scores$se_point.y)
})


test_that("score_model_out succeeds with valid inputs: mean output_type, character metrics, custom by", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |>
      dplyr::filter(.data[["output_type"]] == "mean"),
    oracle_output = forecast_oracle_output,
    metrics = c("ae_point", "se_point"),
    by = c("model_id", "location")
  )

  exp_scores <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "mean") |>
    dplyr::left_join(
      forecast_oracle_output |>
        dplyr::filter(.data[["output_type"]] == "mean"),
      by = c("location", "target_end_date", "target")
    ) |>
    dplyr::mutate(
      ae = abs(.data[["value"]] - .data[["oracle_value"]]),
      se = (.data[["value"]] - .data[["oracle_value"]])^2
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("model_id", "location")
    ))) |>
    dplyr::summarize(
      se_point = mean(.data[["se"]]),
      .groups = "drop"
    )

  # same column names, number of rows, and score values
  expect_equal(colnames(act_scores), colnames(exp_scores))
  expect_equal(nrow(act_scores), nrow(exp_scores))
  merged_scores <- dplyr::full_join(
    act_scores,
    exp_scores,
    by = c("model_id", "location")
  )
  expect_equal(nrow(act_scores), nrow(merged_scores))
  expect_equal(merged_scores$ae_point.x, merged_scores$ae_point.y)
  expect_equal(merged_scores$se_point.x, merged_scores$se_point.y)
})


test_that("score_model_out succeeds with valid inputs: median output_type, default metrics, summarize FALSE", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |>
      dplyr::filter(.data[["output_type"]] == "median"),
    oracle_output = forecast_oracle_output,
    summarize = FALSE
  )

  exp_scores <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "median") |>
    dplyr::left_join(
      forecast_oracle_output |>
        dplyr::filter(.data[["output_type"]] == "median"),
      by = c("location", "target_end_date", "target")
    ) |>
    dplyr::mutate(
      ae = abs(.data[["value"]] - .data[["oracle_value"]])
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c(
        "model_id",
        "reference_date",
        "target",
        "horizon",
        "location",
        "target_end_date"
      )
    ))) |>
    dplyr::summarize(
      ae_point = mean(.data[["ae"]]),
      .groups = "drop"
    )

  # same column names, number of rows, and score values
  expect_equal(colnames(act_scores), colnames(exp_scores))
  expect_equal(nrow(act_scores), nrow(exp_scores))
  merged_scores <- dplyr::full_join(
    act_scores,
    exp_scores,
    by = c(
      "model_id",
      "location",
      "reference_date",
      "horizon",
      "target_end_date",
      "target"
    )
  )
  expect_equal(nrow(act_scores), nrow(merged_scores))
  expect_equal(merged_scores$ae_point.x, merged_scores$ae_point.y)
})


test_that("score_model_out succeeds with valid inputs: quantile output_type, wis and interval metrics, custom by", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |>
      dplyr::filter(.data[["output_type"]] == "quantile"),
    oracle_output = forecast_oracle_output,
    metrics = c("wis", "interval_coverage_80", "interval_coverage_90"),
    by = c("model_id", "location")
  )

  exp_scores <- forecast_outputs |>
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
        .data[["output_type_id"]] *
          (.data[["oracle_value"]] - .data[["value"]]),
        (1 - .data[["output_type_id"]]) *
          (.data[["value"]] - .data[["oracle_value"]])
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
      c(
        "model_id",
        "location",
        "reference_date",
        "horizon",
        "target_end_date",
        "target"
      )
    ))) |>
    dplyr::summarize(
      wis = 2 * mean(.data[["qs"]]),
      interval_coverage_80 = (sum(
        .data[["q_coverage_80_lower"]],
        na.rm = TRUE
      ) ==
        1) *
        (sum(.data[["q_coverage_80_upper"]], na.rm = TRUE) == 1),
      interval_coverage_90 = (sum(
        .data[["q_coverage_90_lower"]],
        na.rm = TRUE
      ) ==
        1) *
        (sum(.data[["q_coverage_90_upper"]], na.rm = TRUE) == 1)
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("model_id", "location")
    ))) |>
    dplyr::summarize(
      wis = mean(.data[["wis"]]),
      interval_coverage_80 = mean(
        .data[["interval_coverage_80"]],
        na.rm = TRUE
      ),
      interval_coverage_90 = mean(
        .data[["interval_coverage_90"]],
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  # same column names, number of rows, and score values
  expect_equal(colnames(act_scores), colnames(exp_scores))
  expect_equal(nrow(act_scores), nrow(exp_scores))
  merged_scores <- dplyr::full_join(
    act_scores,
    exp_scores,
    by = c("model_id", "location")
  )
  expect_equal(nrow(act_scores), nrow(merged_scores))
  expect_equal(merged_scores$wis.x, merged_scores$wis.y)
  expect_equal(
    merged_scores$interval_coverage_80.x,
    merged_scores$interval_coverage_80.y
  )
  expect_equal(
    merged_scores$interval_coverage_90.x,
    merged_scores$interval_coverage_90.y
  )
})


test_that("score_model_out succeeds with valid inputs: quantile output_type, wis/interval metrics, summarize FALSE", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |>
      dplyr::filter(.data[["output_type"]] == "quantile"),
    oracle_output = forecast_oracle_output,
    metrics = c("wis", "interval_coverage_80", "interval_coverage_90"),
    summarize = FALSE
  )

  exp_scores <- forecast_outputs |>
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
        .data[["output_type_id"]] *
          (.data[["oracle_value"]] - .data[["value"]]),
        (1 - .data[["output_type_id"]]) *
          (.data[["value"]] - .data[["oracle_value"]])
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
      c(
        "model_id",
        "reference_date",
        "target",
        "horizon",
        "location",
        "target_end_date"
      )
    ))) |>
    dplyr::summarize(
      wis = 2 * mean(.data[["qs"]]),
      interval_coverage_80 = as.logical(
        (sum(.data[["q_coverage_80_lower"]], na.rm = TRUE) == 1) *
          (sum(.data[["q_coverage_80_upper"]], na.rm = TRUE) == 1)
      ),
      interval_coverage_90 = as.logical(
        (sum(.data[["q_coverage_90_lower"]], na.rm = TRUE) == 1) *
          (sum(.data[["q_coverage_90_upper"]], na.rm = TRUE) == 1)
      ),
      .groups = "drop"
    )

  # same column names, number of rows, and score values
  expect_equal(colnames(act_scores), colnames(exp_scores))
  expect_equal(nrow(act_scores), nrow(exp_scores))
  merged_scores <- dplyr::full_join(
    act_scores,
    exp_scores,
    by = c(
      "model_id",
      "location",
      "reference_date",
      "horizon",
      "target_end_date",
      "target"
    )
  )
  expect_equal(nrow(act_scores), nrow(merged_scores))
  expect_equal(merged_scores$wis.x, merged_scores$wis.y)
  expect_equal(
    merged_scores$interval_coverage_80.x,
    merged_scores$interval_coverage_80.y
  )
  expect_equal(
    merged_scores$interval_coverage_90.x,
    merged_scores$interval_coverage_90.y
  )
})


test_that("score_model_out succeeds with valid inputs: nominal pmf output_type, default metrics, custom by", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |>
      dplyr::filter(.data[["output_type"]] == "pmf"),
    oracle_output = forecast_oracle_output,
    by = c("model_id", "location")
  )

  exp_scores <- read.csv(test_path("testdata", "exp_pmf_scores.csv")) |>
    dplyr::mutate(location = as.character(location)) |>
    dplyr::select(-rps)

  # same column names, number of rows, and score values
  expect_equal(colnames(act_scores), colnames(exp_scores))
  expect_equal(nrow(act_scores), nrow(exp_scores))
  merged_scores <- dplyr::full_join(
    act_scores,
    exp_scores,
    by = c("model_id", "location")
  )
  expect_equal(nrow(act_scores), nrow(merged_scores))
  expect_equal(merged_scores$ae_point.x, merged_scores$ae_point.y)
})


test_that("score_model_out succeeds with valid inputs: ordinal pmf output_type, default metrics, custom by", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |>
      dplyr::filter(.data[["output_type"]] == "pmf"),
    oracle_output = forecast_oracle_output,
    by = c("model_id", "location"),
    output_type_id_order = c("low", "moderate", "high", "very high")
  )

  exp_scores <- read.csv(test_path("testdata", "exp_pmf_scores.csv")) |>
    dplyr::mutate(location = as.character(location))

  # same answer
  expect_equal(act_scores, exp_scores, ignore_attr = TRUE)
})


test_that("score_model_out errors when model_out_tbl has multiple output_types", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs,
      oracle_output = forecast_oracle_output
    ),
    regexp = "model_out_tbl must contain a single output_type, but it has multiple"
  )
})


test_that("score_model_out works with all kinds of interval levels are requested", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |>
        dplyr::filter(.data[["output_type"]] == "quantile"),
      oracle_output = forecast_oracle_output,
      metrics = "interval_coverage_5d2a"
    ),
    regexp = "must be a number between 0 and 100"
  )

  suppressWarnings({
    expect_warning(
      score_model_out(
        model_out_tbl = forecast_outputs |>
          dplyr::filter(.data[["output_type"]] == "quantile"),
        oracle_output = forecast_oracle_output,
        metrics = "interval_coverage_55"
      ),
      "To compute the interval coverage for an interval range of" #scoringutils warning
    )

    expect_error(
      score_model_out(
        model_out_tbl = forecast_outputs |>
          dplyr::filter(.data[["output_type"]] == "quantile"),
        oracle_output = forecast_oracle_output,
        metrics = "interval_coverage_100"
      ),
      regexp = "must be a number between 0 and 100"
    )

    expect_warning(
      score_model_out(
        model_out_tbl = forecast_outputs |>
          dplyr::filter(.data[["output_type"]] == "quantile"),
        oracle_output = forecast_oracle_output,
        metrics = "interval_coverage_5.3"
      ),
      "To compute the interval coverage for an interval range of" #scoringutils warning
    )
  })
})


test_that("score_model_out errors when invalid metrics are requested", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |>
        dplyr::filter(.data[["output_type"]] == "quantile"),
      oracle_output = forecast_oracle_output,
      metrics = "log_score"
    ),
    regexp = "has additional elements"
  )

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |>
        dplyr::filter(.data[["output_type"]] == "quantile"),
      oracle_output = forecast_oracle_output,
      metrics = list(5, 6, "asdf")
    ),
    regexp = paste0(
      "^Assertion on 'c\\(select, exclude\\)' failed: Must be of",
      " type 'character' \\(or 'NULL'\\), not 'list'\\.$"
    )
  )

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |>
        dplyr::filter(.data[["output_type"]] == "quantile"),
      oracle_output = forecast_oracle_output,
      metrics = c("asdfinterval_coverage_90")
    ),
    regexp = "has additional elements"
  )

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |>
        dplyr::filter(.data[["output_type"]] == "mean"),
      oracle_output = forecast_oracle_output,
      metrics = scoringutils::get_metrics(scoringutils::example_point),
      by = c("model_id", "location")
    ),
    regexp = paste0(
      "^Assertion on 'c\\(select, exclude\\)' failed: Must be of",
      " type 'character' \\(or 'NULL'\\), not 'list'\\.$"
    )
  )
})


test_that("score_model_out errors when an unsupported output_type is provided", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |>
        dplyr::filter(.data[["output_type"]] == "cdf"),
      oracle_output = forecast_oracle_output,
      metrics = "log_score"
    ),
    regexp = "only supports the following types"
  )
})


# Tests for scale transformation functionality

test_that("transform argument produces same scores as manually transformed data", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  quantile_data <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "quantile")

  # Manually transform the data using sqrt
  quantile_data_transformed <- quantile_data |>
    dplyr::mutate(value = sqrt(.data[["value"]]))
  oracle_output_transformed <- forecast_oracle_output |>
    dplyr::mutate(oracle_value = sqrt(.data[["oracle_value"]]))

  # Score manually transformed data WITHOUT transform argument
  scores_manual <- score_model_out(
    model_out_tbl = quantile_data_transformed,
    oracle_output = oracle_output_transformed,
    metrics = "wis",
    by = "model_id"
  )

  # Score original data WITH transform argument
  scores_auto <- score_model_out(
    model_out_tbl = quantile_data,
    oracle_output = forecast_oracle_output,
    metrics = "wis",
    transform = sqrt,
    by = "model_id"
  )

  # Both approaches should produce identical scores

  expect_equal(scores_manual$wis, scores_auto$wis)
})


test_that("score_model_out succeeds with log transformation on median forecasts", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  scores <- score_model_out(
    model_out_tbl = forecast_outputs |>
      dplyr::filter(.data[["output_type"]] == "median"),
    oracle_output = forecast_oracle_output,
    transform = scoringutils::log_shift,
    by = "model_id"
  )

  expect_true(nrow(scores) > 0)
  expect_true("ae_point" %in% colnames(scores))
})


test_that("score_model_out succeeds with log transformation on mean forecasts", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  scores <- score_model_out(
    model_out_tbl = forecast_outputs |>
      dplyr::filter(.data[["output_type"]] == "mean"),
    oracle_output = forecast_oracle_output,
    transform = scoringutils::log_shift,
    by = "model_id"
  )

  expect_true(nrow(scores) > 0)
  expect_true("se_point" %in% colnames(scores))
})


test_that("score_model_out with transform_append=TRUE includes both scales", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  scores <- score_model_out(
    model_out_tbl = forecast_outputs |>
      dplyr::filter(.data[["output_type"]] == "quantile"),
    oracle_output = forecast_oracle_output,
    metrics = "wis",
    transform = sqrt,
    transform_append = TRUE,
    summarize = FALSE
  )

  expect_true("scale" %in% colnames(scores))
  expect_true("natural" %in% scores$scale)
  expect_true("sqrt" %in% scores$scale)
})


test_that("score_model_out errors when transform requested for pmf output_type", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |>
        dplyr::filter(.data[["output_type"]] == "pmf"),
      oracle_output = forecast_oracle_output,
      transform = scoringutils::log_shift
    ),
    regexp = "Scale transformations are not supported for pmf output types"
  )
})


test_that("score_model_out errors when transform is not a function", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |>
        dplyr::filter(.data[["output_type"]] == "quantile"),
      oracle_output = forecast_oracle_output,
      transform = "log"
    ),
    regexp = "transform.*must be a function or NULL"
  )
})


test_that("score_model_out errors when transform_label is not character", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |>
        dplyr::filter(.data[["output_type"]] == "quantile"),
      oracle_output = forecast_oracle_output,
      transform = scoringutils::log_shift,
      transform_label = 123
    ),
    regexp = "transform_label.*must be a character string or NULL"
  )
})


test_that("score_model_out passes ... arguments to transform function", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  quantile_data <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "quantile")

  # Using log_shift with offset=1 should avoid NaN scores and warnings
  # (data contains zeros which would cause issues without offset)
  scores <- score_model_out(
    model_out_tbl = quantile_data,
    oracle_output = forecast_oracle_output,
    metrics = "wis",
    transform = scoringutils::log_shift,
    by = "model_id",
    offset = 1
  )

  expect_true(nrow(scores) > 0)
  expect_false(any(is.nan(scores$wis)))
})


test_that("score_model_out propagates warnings from scoringutils", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  # Data contains zeros - log_shift without offset should warn
  quantile_data <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "quantile")

  expect_warning(
    score_model_out(
      model_out_tbl = quantile_data,
      oracle_output = forecast_oracle_output,
      metrics = "wis",
      transform = scoringutils::log_shift,
      by = "model_id"
    ),
    regexp = "Detected zeros in input values"
  )
})


# --- Sample scoring integration tests ---

test_that("score_model_out succeeds with sample output_type, marginal scoring, by model_id", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  scores <- score_model_out(
    model_out_tbl = forecast_outputs |>
      dplyr::filter(.data[["output_type"]] == "sample"),
    oracle_output = forecast_oracle_output,
    metrics = "crps",
    by = "model_id"
  )

  expect_s3_class(scores, c("scores", "data.table", "data.frame"))
  expect_named(scores, c("model_id", "crps"))
  expect_equal(nrow(scores), 3L)
})


test_that("score_model_out succeeds with sample output_type, summarize = FALSE", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  scores <- score_model_out(
    model_out_tbl = forecast_outputs |>
      dplyr::filter(.data[["output_type"]] == "sample"),
    oracle_output = forecast_oracle_output,
    metrics = "crps",
    summarize = FALSE
  )

  expect_s3_class(scores, c("scores", "data.table", "data.frame"))
  expect_named(
    scores,
    c(
      "model_id",
      "reference_date",
      "target",
      "horizon",
      "location",
      "target_end_date",
      "crps"
    )
  )
  # 3 models x 2 reference_dates x 2 locations x 4 horizons = 48
  expect_equal(nrow(scores), 48L)
})


test_that("score_model_out succeeds with sample output_type, default metrics", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  # log_score uses kernel density estimation, which scoringutils correctly
  # warns is not appropriate for integer-valued forecasts
  expect_warning(
    scores <- score_model_out(
      model_out_tbl = forecast_outputs |>
        dplyr::filter(.data[["output_type"]] == "sample"),
      oracle_output = forecast_oracle_output,
      by = "model_id"
    ),
    regexp = "integer-valued"
  )

  expect_s3_class(scores, c("scores", "data.table", "data.frame"))
  expect_named(
    scores,
    c(
      "model_id",
      "bias",
      "dss",
      "crps",
      "overprediction",
      "underprediction",
      "dispersion",
      "log_score",
      "mad",
      "ae_median",
      "se_mean"
    )
  )
  expect_equal(nrow(scores), 3L)
})


test_that("score_model_out computes correct CRPS for marginal sample scoring", {
  # Hand-computed CRPS for 3 samples {1, 3, 5}, observed = 2:
  # CRPS = (1/n) sum|x_i - y| - (1/(2n^2)) sum_ij|x_i - x_j|
  #      = (1/3)(1 + 1 + 3) - (1/18)(2 + 4 + 2 + 2 + 4 + 2) = 5/3 - 8/9 = 7/9
  model_out_tbl <- data.frame(
    model_id = "m1",
    output_type = "sample",
    output_type_id = as.character(1:3),
    value = c(1, 3, 5),
    location = "A",
    target = "inc hosp",
    target_end_date = as.Date("2024-01-01"),
    stringsAsFactors = FALSE
  )

  oracle_output <- data.frame(
    location = "A",
    target = "inc hosp",
    target_end_date = as.Date("2024-01-01"),
    oracle_value = 2,
    stringsAsFactors = FALSE
  )

  scores <- score_model_out(
    model_out_tbl = model_out_tbl,
    oracle_output = oracle_output,
    metrics = "crps",
    summarize = FALSE
  )

  expect_equal(scores$crps, 7 / 9)
})


test_that("score_model_out computes correct energy score for compound sample scoring", {
  # Hand-computed energy score for 3 bivariate samples over 2 horizons.
  # Samples: {(1,2), (3,4), (5,6)}, observed: (2,3)
  # ES = (1/n) sum||x_i - y|| - (1/(2n^2)) sum_ij||x_i - x_j||
  #    = (1/3)(sqrt(2) + sqrt(2) + 3*sqrt(2))
  #      - (1/18)(2*2*sqrt(2) + 2*4*sqrt(2) + 2*2*sqrt(2))
  #    = 5*sqrt(2)/3 - 8*sqrt(2)/9 = 7*sqrt(2)/9
  model_out_tbl <- data.frame(
    model_id = "m1",
    output_type = "sample",
    output_type_id = as.character(c(1, 1, 2, 2, 3, 3)),
    value = c(1, 2, 3, 4, 5, 6),
    location = "A",
    target = "inc hosp",
    horizon = c(1L, 2L, 1L, 2L, 1L, 2L),
    stringsAsFactors = FALSE
  )

  oracle_output <- data.frame(
    location = "A",
    target = "inc hosp",
    horizon = c(1L, 2L),
    oracle_value = c(2, 3),
    stringsAsFactors = FALSE
  )

  scores <- score_model_out(
    model_out_tbl = model_out_tbl,
    oracle_output = oracle_output,
    compound_taskid_set = "location",
    summarize = FALSE
  )

  expect_equal(scores$energy_score, 7 * sqrt(2) / 9)
})


test_that("score_model_out succeeds with compound sample scoring (energy score)", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  # compound_taskid_set matches the hub's tasks.json: reference_date and

  # location stay constant within each draw; horizon varies (joint_across).
  scores <- score_model_out(
    model_out_tbl = forecast_outputs |>
      dplyr::filter(.data[["output_type"]] == "sample"),
    oracle_output = forecast_oracle_output,
    compound_taskid_set = c("reference_date", "location"),
    by = "model_id"
  )

  expect_s3_class(scores, c("scores", "data.table", "data.frame"))
  expect_named(scores, c("model_id", "energy_score"))
  expect_equal(nrow(scores), 3L)
})


test_that("score_model_out succeeds with marginal sample and scale transformation", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  sample_tbl <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "sample")
  # Workaround for stale negative value in hubExamples data
  # (hubverse-org/hubExamples#62). Remove once fixed.
  sample_tbl$value[sample_tbl$value < 0] <- 0

  scores <- score_model_out(
    model_out_tbl = sample_tbl,
    oracle_output = forecast_oracle_output,
    metrics = "crps",
    transform = scoringutils::log_shift,
    offset = 1,
    by = "model_id"
  )

  expect_s3_class(scores, c("scores", "data.table", "data.frame"))
  expect_named(scores, c("model_id", "crps"))
  expect_equal(nrow(scores), 3L)
})


test_that("score_model_out with sample transform_append=TRUE includes both scales", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  sample_tbl <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "sample")
  sample_tbl$value[sample_tbl$value < 0] <- 0

  scores <- score_model_out(
    model_out_tbl = sample_tbl,
    oracle_output = forecast_oracle_output,
    metrics = "crps",
    transform = scoringutils::log_shift,
    offset = 1,
    transform_append = TRUE,
    summarize = FALSE
  )

  expect_s3_class(scores, c("scores", "data.table", "data.frame"))
  expect_named(
    scores,
    c(
      "model_id",
      "reference_date",
      "target",
      "horizon",
      "location",
      "target_end_date",
      "scale",
      "crps"
    )
  )
  # 48 per scale x 2 scales = 96
  expect_equal(nrow(scores), 96L)
  expect_setequal(unique(scores$scale), c("natural", "log_shift"))
})


test_that("score_model_out succeeds with compound sample and scale transformation", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  sample_tbl <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "sample")
  sample_tbl$value[sample_tbl$value < 0] <- 0

  scores <- score_model_out(
    model_out_tbl = sample_tbl,
    oracle_output = forecast_oracle_output,
    compound_taskid_set = c("reference_date", "location"),
    transform = scoringutils::log_shift,
    offset = 1,
    by = "model_id"
  )

  expect_s3_class(scores, c("scores", "data.table", "data.frame"))
  expect_named(scores, c("model_id", "energy_score"))
  expect_equal(nrow(scores), 3L)
})


test_that("score_model_out with compound sample transform_append=TRUE includes both scales", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  sample_tbl <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "sample")
  sample_tbl$value[sample_tbl$value < 0] <- 0

  scores <- score_model_out(
    model_out_tbl = sample_tbl,
    oracle_output = forecast_oracle_output,
    compound_taskid_set = c("reference_date", "location"),
    transform = scoringutils::log_shift,
    offset = 1,
    transform_append = TRUE,
    summarize = FALSE
  )

  expect_s3_class(scores, c("scores", "data.table", "data.frame"))
  expect_named(
    scores,
    c(
      "model_id",
      "reference_date",
      "target",
      "location",
      "scale",
      "energy_score",
      ".mv_group_id"
    )
  )
  # 12 per scale (3 models x 2 ref_dates x 2 locations) x 2 scales = 24
  expect_equal(nrow(scores), 24L)
  expect_setequal(unique(scores$scale), c("natural", "log_shift"))
})


test_that("score_model_out succeeds with sample and relative metrics", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  scores <- score_model_out(
    model_out_tbl = forecast_outputs |>
      dplyr::filter(.data[["output_type"]] == "sample"),
    oracle_output = forecast_oracle_output,
    metrics = "crps",
    relative_metrics = "crps",
    by = "model_id"
  )

  expect_s3_class(scores, c("scores", "data.table", "data.frame"))
  expect_named(scores, c("model_id", "crps", "crps_relative_skill"))
  expect_equal(nrow(scores), 3L)
})


test_that("score_model_out errors when compound_taskid_set used with non-sample type", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |>
        dplyr::filter(.data[["output_type"]] == "quantile"),
      oracle_output = forecast_oracle_output,
      compound_taskid_set = c("reference_date", "location")
    ),
    regexp = "only applicable to sample output types"
  )
})


test_that("error_if_invalid_output_type accepts sample", {
  expect_no_error(error_if_invalid_output_type("sample"))
})
