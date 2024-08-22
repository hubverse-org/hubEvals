# This file uses test fixture data that is created in
# tests/testthat/testdata/make_pmf_test_data.R, and can be loaded by the calls
#
# model_out_tbl <- pmf_test_model_out_tbl()
# target_observations <- pmf_test_target_observations()
# exp_forecast <- pmf_test_exp_forecast()
#
# By construction, model_out_tbl and target_observations are valid inputs to transform_pmf_model_out,
# and exp_forecast is the expected return value from transform_pmf_model_out

test_that("transform_pmf_model_out succeeds with valid inputs", {
  model_out_tbl <- pmf_test_model_out_tbl()
  target_observations <- pmf_test_target_observations()
  exp_forecast <- pmf_test_exp_forecast()

  act_forecast <- transform_pmf_model_out(
    model_out_tbl = model_out_tbl,
    target_observations = target_observations
  )

  expect_equal(act_forecast, exp_forecast)
})
test_that("output_type_id_order is unsupported (fix when supported)", {
  model_out_tbl <- pmf_test_model_out_tbl()
  target_observations <- pmf_test_target_observations()
  exp_forecast <- pmf_test_exp_forecast()

  expect_error(
    act_forecast <- transform_pmf_model_out(
      model_out_tbl = model_out_tbl,
      target_observations = target_observations,
      output_type_id_order = "excellence"
    ),
    regexp = "not yet supported"
  )
})

test_that("transform_pmf_model_out doesn't depend on specific column names for task id variables", {
  model_out_tbl <- pmf_test_model_out_tbl() |>
    dplyr::rename(loc = location, date = target_date)
  target_observations <- pmf_test_target_observations() |>
    dplyr::rename(loc = location, date = target_date)
  exp_forecast <- pmf_test_exp_forecast() |>
    dplyr::rename(loc = location, date = target_date)

  act_forecast <- transform_pmf_model_out(
    model_out_tbl = model_out_tbl,
    target_observations = target_observations
  )

  expect_equal(act_forecast, exp_forecast)
})


test_that("transform_pmf_model_out throws an error if model_out_tbl has no rows", {
  # Error is thrown by checkmate::assert_data_frame() via scoringutils::assert_forecast_generic()
  model_out_tbl <- pmf_test_model_out_tbl()
  target_observations <- pmf_test_target_observations()
  expect_error(
    suppressWarnings(transform_pmf_model_out(
      model_out_tbl = model_out_tbl[0, ],
      target_observations = target_observations
    )),
    regexp = "Must have at least 1 rows, but has 0 rows."
  )
})


test_that("many-to-one relationship exists between model_out_tbl and target_observations", {
  model_out_tbl <- pmf_test_model_out_tbl()
  target_observations <- pmf_test_target_observations() |>
    dplyr::select(-location)

  expect_error(
    suppressMessages(transform_pmf_model_out(
      model_out_tbl = model_out_tbl,
      target_observations = target_observations
    )),
    regexp = "Each row in `x` must match at most 1 row in `y`."
  )
})
