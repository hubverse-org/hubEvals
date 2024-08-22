test_that("inputs are valid", {
  model_out_tbl_1 <- utils::read.csv(
    test_path("testdata/model_out_tbl_point_1.csv")
  )
  expect_error(
    suppressMessages(transform_point_model_out(
      model_out_tbl = model_out_tbl_1,
      target_observations = data.frame(),
      output_type = NULL
    )),
    regexp = "invalid 'output_type': Must be 'mean' or 'median'"
  )
  expect_error(
    suppressMessages(transform_point_model_out(
      model_out_tbl = model_out_tbl_1,
      target_observations = data.frame(),
      output_type = 1
    )),
    regexp = "invalid 'output_type': 1 Must be 'mean' or 'median'"
  )
})


test_that("model_out_tbl_1 output is valid", {
  # Task IDs: location, reference_date, target_end_date, target

  model_out_tbl_1 <- utils::read.csv(
    test_path("testdata/model_out_tbl_point_1.csv")
  )
  target_observations_1 <- utils::read.csv(
    test_path("testdata/target_data_1.csv")
  )
  act_forecast <- transform_point_model_out(
    model_out_tbl = model_out_tbl_1,
    target_observations = target_observations_1,
    output_type = "mean"
  )
  exp_forecast <- utils::read.csv(
    test_path("testdata/exp_forecast_1.csv")
  )
  class(exp_forecast) <- c("forecast_point", "forecast", "data.table", "data.frame")
  expect_equal(act_forecast, exp_forecast)
})


test_that("model_out_tbl_1 columns are valid", {
  # Task IDs: location, reference_date, target_end_date, target

  model_out_tbl_1 <- utils::read.csv(test_path("testdata/model_out_tbl_point_1.csv")) |>
    dplyr::rename(loc = location, trgt = target, date = target_end_date)

  target_observations_1 <- utils::read.csv(test_path("testdata/target_data_1.csv")) |>
    dplyr::rename(loc = location, date = target_end_date)

  act_forecast <- transform_point_model_out(
    model_out_tbl = model_out_tbl_1,
    target_observations = target_observations_1,
    output_type = "mean"
  )
  exp_forecast <- utils::read.csv(test_path("testdata/exp_forecast_1.csv")) |>
    dplyr::rename(loc = location, trgt = target, date = target_end_date)
  class(exp_forecast) <- c("forecast_point", "forecast", "data.table", "data.frame")
  expect_equal(act_forecast, exp_forecast, ignore_attr = "class")
})

test_that("model_out_tbl_1 has any rows", {
  # Error is thrown by checkmate::assert_data_frame() via scoringutils::assert_forecast_generic()
  model_out_tbl_1 <- utils::read.csv(
    test_path("testdata/model_out_tbl_point_1.csv")
  )
  target_observations_1 <- utils::read.csv(
    test_path("testdata/target_data_1.csv")
  )
  expect_error(
    suppressWarnings(transform_point_model_out(
      model_out_tbl = model_out_tbl_1[0, ],
      target_observations = target_observations_1,
      output_type = "mean"
    )),
    regexp = "Must have at least 1 rows, but has 0 rows."
  )
})


test_that("many-to-one relationship exists between model_out_tbl and target_observations", {
  model_out_tbl_1 <- utils::read.csv(
    test_path("testdata/model_out_tbl_point_1.csv")
  )
  target_observations_2 <- utils::read.csv(
    test_path("testdata/target_data_2.csv")
  )

  expect_error(
    suppressMessages(transform_point_model_out(
      model_out_tbl = model_out_tbl_1,
      target_observations = target_observations_2,
      output_type = "mean"
    )),
    regexp = "Each row in `x` must match at most 1 row in `y`."
  )
})

test_that("hubExamples data set is transformed correctly", {
  # forecast_outputs.rda & forecast_target_observations.rda are stored in hubExamples:
  # https://github.com/hubverse-org/hubExamples/tree/main
  load(test_path("testdata/forecast_outputs.rda")) # sets forecast_outputs
  load(test_path("testdata/forecast_target_observations.rda")) # sets forecast_target_observations
  model_out_tbl <- forecast_outputs
  target_observations <- forecast_target_observations
  act_forecast <- transform_point_model_out(
    model_out_tbl = model_out_tbl,
    target_observations = target_observations,
    output_type = "mean"
  )

  exp_forecast <- utils::read.csv(
    test_path("testdata/exp_forecast_hubExamples_1.csv")
  ) |>
    dplyr::mutate(
      location = as.character(location),
      reference_date = as.Date(reference_date, "%Y-%m-%d"),
      target_end_date = as.Date(target_end_date, "%Y-%m-%d")
    )
  class(exp_forecast) <- c("forecast_point", "forecast", "data.table", "data.frame")
  expect_equal(act_forecast, exp_forecast)
})
