test_that("score_model_out succeeds with valid inputs: mean output_type, default metrics, by all", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "mean"),
    oracle_output = forecast_oracle_output,
    by = c("model_id", "location", "reference_date", "horizon", "target_end_date", "target")
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
      c("model_id", "location", "reference_date", "horizon", "target_end_date", "target")
    ))) |>
    dplyr::summarize(
      se_point = mean(.data[["se"]]),
      .groups = "drop"
    )

  # same column names, number of rows, and score values
  expect_equal(colnames(act_scores), colnames(exp_scores))
  expect_equal(nrow(act_scores), nrow(exp_scores))
  merged_scores <- dplyr::full_join(
    act_scores, exp_scores,
    by = c("model_id", "location", "reference_date", "horizon", "target_end_date", "target")
  )
  expect_equal(nrow(act_scores), nrow(merged_scores))
  expect_equal(merged_scores$se_point.x, merged_scores$se_point.y)
})


test_that("score_model_out succeeds with valid inputs: mean output_type, default metrics, summarize FALSE", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "mean"),
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
      c("model_id", "reference_date", "target", "horizon", "location", "target_end_date")
    ))) |>
    dplyr::summarize(
      se_point = mean(.data[["se"]]),
      .groups = "drop"
    )

  # same column names, number of rows, and score values
  expect_equal(colnames(act_scores), colnames(exp_scores))
  expect_equal(nrow(act_scores), nrow(exp_scores))
  merged_scores <- dplyr::full_join(
    act_scores, exp_scores,
    by = c("model_id", "location", "reference_date", "horizon", "target_end_date", "target")
  )
  expect_equal(nrow(act_scores), nrow(merged_scores))
  expect_equal(merged_scores$se_point.x, merged_scores$se_point.y)
})


test_that("score_model_out succeeds with valid inputs: mean output_type, character metrics, custom by", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "mean"),
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
    act_scores, exp_scores,
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
    model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "median"),
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
      c("model_id", "reference_date", "target", "horizon", "location", "target_end_date")
    ))) |>
    dplyr::summarize(
      ae_point = mean(.data[["ae"]]),
      .groups = "drop"
    )

  # same column names, number of rows, and score values
  expect_equal(colnames(act_scores), colnames(exp_scores))
  expect_equal(nrow(act_scores), nrow(exp_scores))
  merged_scores <- dplyr::full_join(
    act_scores, exp_scores,
    by = c("model_id", "location", "reference_date", "horizon", "target_end_date", "target")
  )
  expect_equal(nrow(act_scores), nrow(merged_scores))
  expect_equal(merged_scores$ae_point.x, merged_scores$ae_point.y)
})


test_that("score_model_out succeeds with valid inputs: quantile output_type, wis and interval metrics, custom by", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
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
      wis = 2 * mean(.data[["qs"]]),
      interval_coverage_80 = (sum(.data[["q_coverage_80_lower"]], na.rm = TRUE) == 1) *
        (sum(.data[["q_coverage_80_upper"]], na.rm = TRUE) == 1),
      interval_coverage_90 = (sum(.data[["q_coverage_90_lower"]], na.rm = TRUE) == 1) *
        (sum(.data[["q_coverage_90_upper"]], na.rm = TRUE) == 1)
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("model_id", "location")
    ))) |>
    dplyr::summarize(
      wis = mean(.data[["wis"]]),
      interval_coverage_80 = mean(.data[["interval_coverage_80"]], na.rm = TRUE),
      interval_coverage_90 = mean(.data[["interval_coverage_90"]], na.rm = TRUE),
      .groups = "drop"
    )

  # same column names, number of rows, and score values
  expect_equal(colnames(act_scores), colnames(exp_scores))
  expect_equal(nrow(act_scores), nrow(exp_scores))
  merged_scores <- dplyr::full_join(
    act_scores, exp_scores,
    by = c("model_id", "location")
  )
  expect_equal(nrow(act_scores), nrow(merged_scores))
  expect_equal(merged_scores$wis.x, merged_scores$wis.y)
  expect_equal(merged_scores$interval_coverage_80.x, merged_scores$interval_coverage_80.y)
  expect_equal(merged_scores$interval_coverage_90.x, merged_scores$interval_coverage_90.y)
})


test_that("score_model_out succeeds with valid inputs: quantile output_type, wis/interval metrics, summarize FALSE", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
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
      c("model_id", "reference_date", "target", "horizon", "location", "target_end_date")
    ))) |>
    dplyr::summarize(
      wis = 2 * mean(.data[["qs"]]),
      interval_coverage_80 = as.logical((sum(.data[["q_coverage_80_lower"]], na.rm = TRUE) == 1) *
                                          (sum(.data[["q_coverage_80_upper"]], na.rm = TRUE) == 1)),
      interval_coverage_90 = as.logical((sum(.data[["q_coverage_90_lower"]], na.rm = TRUE) == 1) *
                                          (sum(.data[["q_coverage_90_upper"]], na.rm = TRUE) == 1)),
      .groups = "drop"
    )

  # same column names, number of rows, and score values
  expect_equal(colnames(act_scores), colnames(exp_scores))
  expect_equal(nrow(act_scores), nrow(exp_scores))
  merged_scores <- dplyr::full_join(
    act_scores, exp_scores,
    by = c("model_id", "location", "reference_date", "horizon", "target_end_date", "target")
  )
  expect_equal(nrow(act_scores), nrow(merged_scores))
  expect_equal(merged_scores$wis.x, merged_scores$wis.y)
  expect_equal(merged_scores$interval_coverage_80.x, merged_scores$interval_coverage_80.y)
  expect_equal(merged_scores$interval_coverage_90.x, merged_scores$interval_coverage_90.y)
})


test_that("score_model_out succeeds with valid inputs: nominal pmf output_type, default metrics, custom by", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "pmf"),
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
    act_scores, exp_scores,
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
      model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
      oracle_output = forecast_oracle_output,
      metrics = "interval_coverage_5d2a"
    ),
    regexp = "must be a number between 0 and 100"
  )

  suppressWarnings({
    expect_warning(
      score_model_out(
        model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
        oracle_output = forecast_oracle_output,
        metrics = "interval_coverage_55"
      ),
      "To compute the interval coverage for an interval range of" #scoringutils warning
    )

    expect_error(
      score_model_out(
        model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
        oracle_output = forecast_oracle_output,
        metrics = "interval_coverage_100"
      ),
      regexp = "must be a number between 0 and 100"
    )

    expect_warning(
      score_model_out(
        model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
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
      model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
      oracle_output = forecast_oracle_output,
      metrics = "log_score"
    ),
    regexp = "has additional elements"
  )

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
      oracle_output = forecast_oracle_output,
      metrics = list(5, 6, "asdf")
    ),
    regexp =
      "^Assertion on 'c\\(select, exclude\\)' failed: Must be of type 'character' \\(or 'NULL'\\), not 'list'\\.$"
  )

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
      oracle_output = forecast_oracle_output,
      metrics = c("asdfinterval_coverage_90")
    ),
    regexp =
      "has additional elements"
  )

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "mean"),
      oracle_output = forecast_oracle_output,
      metrics = scoringutils::get_metrics(scoringutils::example_point),
      by = c("model_id", "location")
    ),
    regexp =
      "^Assertion on 'c\\(select, exclude\\)' failed: Must be of type 'character' \\(or 'NULL'\\), not 'list'\\.$"
  )
})



test_that("score_model_out errors when an unsupported output_type is provided", {
  # Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "cdf"),
      oracle_output = forecast_oracle_output,
      metrics = "log_score"
    ),
    regexp = "only supports the following types"
  )
})
