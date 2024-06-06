test_that("inputs are valid", {
  expect_error(
    transform_quantile_model_out(
      model_out_tbl = NULL,
      target_data = NULL
    )
  )
  expect_error(
    suppressMessages(transform_quantile_model_out(
      model_out_tbl = data.frame(),
      target_data = NULL
    ))
  )
})

test_that("model_out_tbl_1 output is valid", {
  model_out_tbl_1 <- utils::read.csv("testdata/model_out_tbl_quantile_1.csv")
  target_data_1 <- utils::read.csv("testdata/target_data_1.csv")
  act_forecast <- transform_quantile_model_out(
    model_out_tbl = model_out_tbl_1,
    target_data = target_data_1
  )

  exp_forecast <- utils::read.csv("testdata/exp_forecast_2.csv")
  class(exp_forecast) <- c("forecast_quantile", "data.table", "data.frame")
  expect_equal(act_forecast, exp_forecast)
})

test_that("test target_data has observation column", {
  model_out_tbl_1 <- utils::read.csv("testdata/model_out_tbl_quantile_1.csv")
  target_data_1 <- utils::read.csv("testdata/target_data_1.csv") %>%
    dplyr::select(-c("observation"))
  expect_error(
    suppressMessages(transform_quantile_model_out(
      model_out_tbl = model_out_tbl_1,
      target_data = target_data_1
    )),
    regexp = "target_data does not have observation column"
  )
})


test_that("model_out_tbl_1 columns are valid", {
  model_out_tbl_1 <- utils::read.csv("testdata/model_out_tbl_quantile_1.csv") %>%
    dplyr::rename(loc = location, trgt = target, date = target_end_date)

  target_data_1 <- utils::read.csv("testdata/target_data_1.csv")
  act_forecast <- transform_quantile_model_out(
    model_out_tbl = model_out_tbl_1,
    target_data = target_data_1
  )
  exp_forecast <- utils::read.csv("testdata/exp_forecast_2.csv") %>%
    dplyr::rename(loc = location, trgt = target, date = target_end_date)
  class(exp_forecast) <- c("forecast_quantile", "data.table", "data.frame")
  expect_equal(act_forecast, exp_forecast)

  # Error when missing any of: model_id, output_type, output_type_id, value
  expect_error(
    suppressMessages(transform_quantile_model_out(
      model_out_tbl = data.frame(),
      target_data = data.frame()
    )),
    regexp = "model_out_tbl does not contain required columns"
  )
})

test_that("model_out_tbl_1 has any rows", {
  # Error is thrown by hubUtils::as_model_out_tbl()
  model_out_tbl_1 <- utils::read.csv("testdata/model_out_tbl_quantile_1.csv")
  target_data_1 <- utils::read.csv("testdata/target_data_1.csv")
  expect_error(
    suppressWarnings(transform_quantile_model_out(
      model_out_tbl = model_out_tbl_1[0, ],
      target_data = target_data_1
    )),
    regexp = "Must have at least 1 rows, but has 0 rows."
  )
})

test_that("model_out_tbl columns match target_data columns", {
  model_out_tbl_1 <- utils::read.csv("testdata/model_out_tbl_quantile_1.csv")
  target_data_1 <- utils::read.csv("testdata/target_data_2.csv")

  expect_error(
    suppressMessages(transform_quantile_model_out(
      model_out_tbl = model_out_tbl_1 %>%
        dplyr::select(-c("location")),
      target_data = target_data_1
    )),
    regexp = "model_out_tbl and target_data do not have compatible columns"
  )
})

test_that("many-to-one relationship exists between model_out_tbl and target_data", {
  model_out_tbl_1 <- utils::read.csv("testdata/model_out_tbl_quantile_1.csv")
  target_data_2 <- utils::read.csv("testdata/target_data_2.csv")

  expect_error(
    suppressMessages(transform_quantile_model_out(
      model_out_tbl = model_out_tbl_1,
      target_data = target_data_2
    )),
    regexp = "Each row in `x` must match at most 1 row in `y`."
  )
})

test_that("hubExamples data set is transformed correctly", {
  # forecast_outputs.rda & forecast_target_observations.rda are stored in hubExamples:
  # https://github.com/Infectious-Disease-Modeling-Hubs/hubExamples/tree/main
  load("testdata/forecast_outputs.rda") # sets forecast_outputs
  load("testdata/forecast_target_observations.rda") # sets forecast_target_observations
  model_out_tbl <- forecast_outputs
  target_data <- forecast_target_observations
  act_forecast <- transform_quantile_model_out(
    model_out_tbl = model_out_tbl,
    target_data = target_data
  )

  exp_forecast <- utils::read.csv("testdata/exp_forecast_hubExamples_2.csv") %>%
    dplyr::mutate(
      location = as.character(location),
      reference_date = as.Date(reference_date, "%Y-%m-%d"),
      target_end_date = as.Date(target_end_date, "%Y-%m-%d")
    )
  class(exp_forecast) <- c("forecast_quantile", "data.table", "data.frame")
  expect_equal(act_forecast, exp_forecast)
})
