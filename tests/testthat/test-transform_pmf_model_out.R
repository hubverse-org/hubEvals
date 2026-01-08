# This file uses test fixture data that is created in
# tests/testthat/testdata/make_pmf_test_data.R, and can be loaded by the calls
#
# model_out_tbl <- pmf_test_model_out_tbl()
# oracle_output <- pmf_test_oracle_output()
# exp_forecast <- pmf_test_exp_forecast()
#
# By construction, model_out_tbl and oracle_output are valid inputs to transform_pmf_model_out,
# and exp_forecast is the expected return value from transform_pmf_model_out

test_that("transform_pmf_model_out succeeds with valid inputs -- nominal", {
  model_out_tbl <- pmf_test_model_out_tbl()
  oracle_output <- pmf_test_oracle_output()
  exp_forecast <- pmf_test_exp_forecast()

  act_forecast <- transform_pmf_model_out(
    model_out_tbl = model_out_tbl,
    oracle_output = oracle_output
  )

  expect_equal(act_forecast, exp_forecast)
})


test_that("transform_pmf_model_out succeeds with valid inputs -- ordinal", {
  model_out_tbl <- pmf_test_model_out_tbl()
  oracle_output <- pmf_test_oracle_output()
  exp_forecast <- pmf_test_exp_forecast() |>
    dplyr::mutate(
      predicted_label = factor(
        predicted_label,
        levels = c("cat", "dog", "bird"),
        ordered = TRUE
      ),
      observed = factor(
        observed,
        levels = c("cat", "dog", "bird"),
        ordered = TRUE
      )
    )
  class(exp_forecast) <- c(
    "forecast_ordinal",
    "forecast",
    "data.table",
    "data.frame"
  )

  act_forecast <- transform_pmf_model_out(
    model_out_tbl = model_out_tbl,
    oracle_output = oracle_output,
    output_type_id_order = c("cat", "dog", "bird")
  )

  expect_equal(act_forecast, exp_forecast)
})


test_that("transform_pmf_model_out throws an error with invalid output_type_id_order", {
  model_out_tbl <- pmf_test_model_out_tbl()
  oracle_output <- pmf_test_oracle_output()

  expect_error(
    act_forecast <- transform_pmf_model_out(
      model_out_tbl = model_out_tbl,
      oracle_output = oracle_output,
      output_type_id_order = c("cat", "bird", "platypus"),
      "`output_type_id_order` must align with the set of all unique `output_type_id` values in `model_out_tbl`."
    )
  )
})


test_that("transform_pmf_model_out doesn't depend on specific column names for task id variables", {
  model_out_tbl <- pmf_test_model_out_tbl() |>
    dplyr::rename(loc = location, date = target_date)
  oracle_output <- pmf_test_oracle_output() |>
    dplyr::rename(loc = location, date = target_date)
  exp_forecast <- pmf_test_exp_forecast() |>
    dplyr::rename(loc = location, date = target_date)

  act_forecast <- transform_pmf_model_out(
    model_out_tbl = model_out_tbl,
    oracle_output = oracle_output
  )

  expect_equal(act_forecast, exp_forecast)
})


test_that("transform_pmf_model_out throws an error if model_out_tbl has no rows", {
  # Error is thrown by checkmate::assert_data_frame() via scoringutils::assert_forecast_generic()
  model_out_tbl <- pmf_test_model_out_tbl()
  oracle_output <- pmf_test_oracle_output()
  expect_error(
    suppressWarnings(transform_pmf_model_out(
      model_out_tbl = model_out_tbl[0, ],
      oracle_output = oracle_output
    )),
    regexp = "Must have at least 1 rows, but has 0 rows."
  )
})


test_that("many-to-one relationship exists between model_out_tbl and oracle_output", {
  model_out_tbl <- pmf_test_model_out_tbl()
  oracle_output <- pmf_test_oracle_output() |>
    dplyr::select(-location)

  expect_error(
    suppressMessages(transform_pmf_model_out(
      model_out_tbl = model_out_tbl,
      oracle_output = oracle_output
    )),
    regexp = "Each row in `x` must match at most 1 row in `y`."
  )
})
