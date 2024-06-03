
test_that("test invalid inputs", {
  expect_error(
    transform_point_model_out(model_out_tbl = NULL, target_data = NULL, output_type = "mean")
  )
  expect_error(
    suppressMessages(transform_point_model_out(model_out_tbl = data.frame(), target_data = NULL, output_type = "mean"))
  )
  expect_error(
    suppressMessages(transform_point_model_out(model_out_tbl = data.frame(), target_data = data.frame(), output_type = "mean"))
  )

  model_out_tbl_1 <- utils::read.csv("testdata/model_out_tbl_point_1.csv")
  expect_error(
    suppressMessages(transform_point_model_out(model_out_tbl = hubUtils::as_model_out_tbl(model_out_tbl_1),
                                               target_data = data.frame(),
                                               output_type = NULL)),
    regexp = "invalid 'output_type': Must be 'mean' or 'median'"
  )
  expect_error(
    suppressMessages(transform_point_model_out(model_out_tbl = hubUtils::as_model_out_tbl(model_out_tbl_1),
                                               target_data = data.frame(),
                                               output_type = 1)),
    regexp = "invalid 'output_type': 1 Must be 'mean' or 'median'"
  )
})


test_that("test model_out_tbl_1 output", {
 # Task IDs: location, reference_date, target_end_date, target

  model_out_tbl_1 <- utils::read.csv("testdata/model_out_tbl_point_1.csv")
  target_data_1 <- utils::read.csv("testdata/target_data_1.csv")
  act_forecast <- transform_point_model_out(model_out_tbl = hubUtils::as_model_out_tbl(model_out_tbl_1),
                                               target_data = target_data_1,
                                               output_type = "mean")
  exp_forecast <- utils::read.csv("testdata/exp_forecast_1.csv")
  class(exp_forecast) <- c("forecast_point","data.table","data.frame")
  expect_equal(act_forecast, exp_forecast)
})
