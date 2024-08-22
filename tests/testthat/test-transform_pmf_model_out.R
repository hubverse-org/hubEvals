# This file uses test fixture data in the following three objects:
# tests/testthat/testdata/pmf_test_model_out_tbl.rds
# tests/testthat/testdata/pmf_test_target_observations.rds
# tests/testthat/testdata/pmf_test_expected_merged_forecast.rds
#
# These files are constructed "by hand" in tests/testthat/testdata/make_pmf_test_data.R
# By construction, model_out_tbl and target_observations are valid inputs to transform_pmf_model_out,
# and exp_forecast is the expected return value from transform_pmf_model_out

test_that("model_out_tbl_1 output is valid", {
  model_out_tbl <- readRDS(test_path("testdata", "pmf_test_model_out_tbl.rds"))
  target_observations <- readRDS(test_path("testdata", "pmf_test_target_observations.rds"))
  exp_forecast <- readRDS(test_path("testdata", "pmf_test_expected_merged_forecast.rds"))

  act_forecast <- transform_pmf_model_out(
    model_out_tbl = model_out_tbl,
    target_observations = target_observations
  )

  expect_equal(act_forecast, exp_forecast, ignore_attr = "groups")
})


test_that("model_out_tbl_1 output doesn't depend on specific column names for task id variables", {
  model_out_tbl <- readRDS(test_path("testdata", "pmf_test_model_out_tbl.rds")) |>
    dplyr::rename(loc = location, date = target_date)
  target_observations <- readRDS(test_path("testdata", "pmf_test_target_observations.rds")) |>
    dplyr::rename(loc = location, date = target_date)
  exp_forecast <- readRDS(test_path("testdata", "pmf_test_expected_merged_forecast.rds")) |>
    dplyr::rename(loc = location, date = target_date)

  act_forecast <- transform_pmf_model_out(
    model_out_tbl = model_out_tbl,
    target_observations = target_observations
  )

  expect_equal(act_forecast, exp_forecast, ignore_attr = "groups")
})


test_that("model_out_tbl_1 has any rows", {
  # Error is thrown by scoringutils::assert_forecast_generic()
  model_out_tbl <- readRDS(test_path("testdata", "pmf_test_model_out_tbl.rds"))
  target_observations <- readRDS(test_path("testdata", "pmf_test_target_observations.rds"))
  expect_error(
    suppressWarnings(transform_pmf_model_out(
      model_out_tbl = model_out_tbl[0, ],
      target_observations = target_observations
    )),
    regexp = "Must have at least 1 rows, but has 0 rows."
  )
})


test_that("many-to-one relationship exists between model_out_tbl and target_observations", {
  model_out_tbl <- readRDS(test_path("testdata", "pmf_test_model_out_tbl.rds"))
  target_observations <- readRDS(test_path("testdata", "pmf_test_target_observations.rds")) |>
    dplyr::select(-location)

  expect_error(
    suppressMessages(transform_pmf_model_out(
      model_out_tbl = model_out_tbl,
      target_observations = target_observations
    )),
    regexp = "Each row in `x` must match at most 1 row in `y`."
  )
})
