test_that("score_model_out succeeds with valid inputs: mean output_type, default metrics, by all", {
  # Forecast data from HubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  load(test_path("testdata/forecast_outputs.rda")) # sets forecast_outputs
  load(test_path("testdata/forecast_target_observations.rda")) # sets forecast_target_observations

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "mean"),
    target_observations = forecast_target_observations,
    by = c("model_id", "location", "reference_date", "horizon", "target_end_date", "target")
  )

  exp_scores <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "mean") |>
    dplyr::left_join(
      forecast_target_observations |>
        dplyr::filter(.data[["output_type"]] == "mean"),
      by = c("location", "target_end_date", "target")
    ) |>
    dplyr::mutate(
      se = (.data[["value"]] - .data[["observation"]])^2
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
  # Forecast data from HubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  load(test_path("testdata/forecast_outputs.rda")) # sets forecast_outputs
  load(test_path("testdata/forecast_target_observations.rda")) # sets forecast_target_observations

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "mean"),
    target_observations = forecast_target_observations,
    summarize = FALSE
  )

  exp_scores <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "mean") |>
    dplyr::left_join(
      forecast_target_observations |>
        dplyr::filter(.data[["output_type"]] == "mean"),
      by = c("location", "target_end_date", "target")
    ) |>
    dplyr::mutate(
      se = (.data[["value"]] - .data[["observation"]])^2
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


test_that("score_model_out succeeds with valid inputs: mean output_type, character metrics, custom by", {
   # Forecast data from HubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
  load(test_path("testdata/forecast_outputs.rda")) # sets forecast_outputs
  load(test_path("testdata/forecast_target_observations.rda")) # sets forecast_target_observations

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "mean"),
    target_observations = forecast_target_observations,
    metrics = c("ae_point", "se_point"),
    by = c("model_id", "location")
  )

  exp_scores <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "mean") |>
    dplyr::left_join(
      forecast_target_observations |>
        dplyr::filter(.data[["output_type"]] == "mean"),
      by = c("location", "target_end_date", "target")
    ) |>
    dplyr::mutate(
      ae = abs(.data[["value"]] - .data[["observation"]]),
      se = (.data[["value"]] - .data[["observation"]])^2
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("model_id", "location")
    ))) |>
    dplyr::summarize(
      ae_point = mean(.data[["ae"]]),
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


test_that("score_model_out succeeds with valid inputs: mean output_type, function metrics, custom by", {
  load(test_path("testdata/forecast_outputs.rda")) # sets forecast_outputs
  load(test_path("testdata/forecast_target_observations.rda")) # sets forecast_target_observations

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "mean"),
    target_observations = forecast_target_observations,
    metrics = scoringutils::metrics_point(),
    by = c("model_id", "location")
  )

  exp_scores <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "mean") |>
    dplyr::left_join(
      forecast_target_observations |>
        dplyr::filter(.data[["output_type"]] == "mean"),
      by = c("location", "target_end_date", "target")
    ) |>
    dplyr::mutate(
      ae = abs(.data[["value"]] - .data[["observation"]]),
      se = (.data[["value"]] - .data[["observation"]])^2,
      pe = abs(.data[["value"]] - .data[["observation"]]) / abs(.data[["observation"]])
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("model_id", "location")
    ))) |>
    dplyr::summarize(
      ae_point = mean(.data[["ae"]]),
      se_point = mean(.data[["se"]]),
      ape = mean(.data[["pe"]]),
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
  expect_equal(merged_scores$ape.x, merged_scores$ape.y)
})


test_that("score_model_out succeeds with valid inputs: median output_type, default metrics, summarize FALSE", {
  load(test_path("testdata/forecast_outputs.rda")) # sets forecast_outputs
  load(test_path("testdata/forecast_target_observations.rda")) # sets forecast_target_observations

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "median"),
    target_observations = forecast_target_observations,
    summarize = FALSE
  )

  exp_scores <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "median") |>
    dplyr::left_join(
      forecast_target_observations |>
        dplyr::filter(.data[["output_type"]] == "median"),
      by = c("location", "target_end_date", "target")
    ) |>
    dplyr::mutate(
      ae = abs(.data[["value"]] - .data[["observation"]])
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("model_id", "location", "reference_date", "horizon", "target_end_date", "target")
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
  load(test_path("testdata/forecast_outputs.rda")) # sets forecast_outputs
  load(test_path("testdata/forecast_target_observations.rda")) # sets forecast_target_observations

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
    target_observations = forecast_target_observations,
    metrics = c("wis", "interval_coverage_80", "interval_coverage_90"),
    by = c("model_id", "location")
  )

  exp_scores <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "quantile") |>
    dplyr::left_join(
      forecast_target_observations |>
        dplyr::filter(.data[["output_type"]] == "quantile") |>
        dplyr::select(-dplyr::all_of(c("output_type", "output_type_id"))),
      by = c("location", "target_end_date", "target")
    ) |>
    dplyr::mutate(
      output_type_id = as.numeric(.data[["output_type_id"]]),
      qs = ifelse(
        .data[["observation"]] >= .data[["value"]],
        .data[["output_type_id"]] * (.data[["observation"]] - .data[["value"]]),
        (1 - .data[["output_type_id"]]) * (.data[["value"]] - .data[["observation"]])
      ),
      q_coverage_80_lower = ifelse(
        .data[["output_type_id"]] == 0.1,
        .data[["observation"]] >= .data[["value"]],
        NA_real_
      ),
      q_coverage_80_upper = ifelse(
        .data[["output_type_id"]] == 0.9,
        .data[["observation"]] <= .data[["value"]],
        NA_real_
      ),
      q_coverage_90_lower = ifelse(
        .data[["output_type_id"]] == 0.05,
        .data[["observation"]] >= .data[["value"]],
        NA_real_
      ),
      q_coverage_90_upper = ifelse(
        .data[["output_type_id"]] == 0.95,
        .data[["observation"]] <= .data[["value"]],
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
  load(test_path("testdata/forecast_outputs.rda")) # sets forecast_outputs
  load(test_path("testdata/forecast_target_observations.rda")) # sets forecast_target_observations

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
    target_observations = forecast_target_observations,
    metrics = c("wis", "interval_coverage_80", "interval_coverage_90"),
    summarize = FALSE
  )

  exp_scores <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "quantile") |>
    dplyr::left_join(
      forecast_target_observations |>
        dplyr::filter(.data[["output_type"]] == "quantile") |>
        dplyr::select(-dplyr::all_of(c("output_type", "output_type_id"))),
      by = c("location", "target_end_date", "target")
    ) |>
    dplyr::mutate(
      output_type_id = as.numeric(.data[["output_type_id"]]),
      qs = ifelse(
        .data[["observation"]] >= .data[["value"]],
        .data[["output_type_id"]] * (.data[["observation"]] - .data[["value"]]),
        (1 - .data[["output_type_id"]]) * (.data[["value"]] - .data[["observation"]])
      ),
      q_coverage_80_lower = ifelse(
        .data[["output_type_id"]] == 0.1,
        .data[["observation"]] >= .data[["value"]],
        NA_real_
      ),
      q_coverage_80_upper = ifelse(
        .data[["output_type_id"]] == 0.9,
        .data[["observation"]] <= .data[["value"]],
        NA_real_
      ),
      q_coverage_90_lower = ifelse(
        .data[["output_type_id"]] == 0.05,
        .data[["observation"]] >= .data[["value"]],
        NA_real_
      ),
      q_coverage_90_upper = ifelse(
        .data[["output_type_id"]] == 0.95,
        .data[["observation"]] <= .data[["value"]],
        NA_real_
      )
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("model_id", "location", "reference_date", "horizon", "target_end_date", "target")
    ))) |>
    dplyr::summarize(
      wis = 2 * mean(.data[["qs"]]),
      interval_coverage_80 = as.logical((sum(.data[["q_coverage_80_lower"]], na.rm = TRUE) == 1) *
                                          (sum(.data[["q_coverage_80_upper"]], na.rm = TRUE) == 1)),
      interval_coverage_90 = as.logical((sum(.data[["q_coverage_90_lower"]], na.rm = TRUE) == 1) *
                                          (sum(.data[["q_coverage_90_upper"]], na.rm = TRUE) == 1))
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
  load(test_path("testdata/forecast_outputs.rda")) # sets forecast_outputs
  load(test_path("testdata/forecast_target_observations.rda")) # sets forecast_target_observations

  act_scores <- score_model_out(
    model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "pmf"),
    target_observations = forecast_target_observations,
    by = c("model_id", "location")
  )

  exp_scores <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "pmf") |>
    dplyr::left_join(
      forecast_target_observations |>
        dplyr::filter(.data[["output_type"]] == "pmf") |>
        dplyr::select(-dplyr::all_of(c("output_type"))),
      by = c("location", "target_end_date", "target", "output_type_id")
    ) |>
    dplyr::filter(.data[["observation"]] == 1) |>
    dplyr::mutate(
      log_score = -1 * log(.data[["value"]])
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("model_id", "location")
    ))) |>
    dplyr::summarize(
      log_score = mean(.data[["log_score"]]),
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
})


test_that("score_model_out errors when model_out_tbl has multiple output_types", {
  load(test_path("testdata/forecast_outputs.rda")) # sets forecast_outputs
  load(test_path("testdata/forecast_target_observations.rda")) # sets forecast_target_observations

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs,
      target_observations = forecast_target_observations
    ),
    regexp = "model_out_tbl must contain a single output_type, but it has multiple"
  )
})


test_that("score_model_out errors when invalid interval levels are requested", {
  load(test_path("testdata/forecast_outputs.rda")) # sets forecast_outputs
  load(test_path("testdata/forecast_target_observations.rda")) # sets forecast_target_observations

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
      target_observations = forecast_target_observations,
      metrics = "interval_level_5"
    ),
    regexp = "unsupported metric"
  )

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
      target_observations = forecast_target_observations,
      metrics = "interval_level_100"
    ),
    regexp = "unsupported metric"
  )

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
      target_observations = forecast_target_observations,
      metrics = "interval_level_XY"
    ),
    regexp = "unsupported metric"
  )
})


test_that("score_model_out errors when invalid metrics are requested", {
  load(test_path("testdata/forecast_outputs.rda")) # sets forecast_outputs
  load(test_path("testdata/forecast_target_observations.rda")) # sets forecast_target_observations

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
      target_observations = forecast_target_observations,
      metrics = "log_score"
    ),
    regexp = "unsupported metric"
  )

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "quantile"),
      target_observations = forecast_target_observations,
      metrics = list(5, 6, "asdf")
    ),
    regexp = "Assertion on 'metrics' failed"
  )
})



test_that("score_model_out errors when an unsupported output_type is provided", {
  load(test_path("testdata/forecast_outputs.rda")) # sets forecast_outputs
  load(test_path("testdata/forecast_target_observations.rda")) # sets forecast_target_observations

  expect_error(
    score_model_out(
      model_out_tbl = forecast_outputs |> dplyr::filter(.data[["output_type"]] == "cdf"),
      target_observations = forecast_target_observations,
      metrics = "log_score"
    ),
    regexp = "only supports the following types"
  )
})
