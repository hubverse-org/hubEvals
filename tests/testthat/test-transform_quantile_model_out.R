test_that("model_out_tbl_1 output is valid", {
  model_out_tbl_1 <- utils::read.csv(test_path("testdata/model_out_tbl_quantile_1.csv"))
  oracle_output_1 <- utils::read.csv(test_path("testdata/target_data_1.csv")) |>
    dplyr::rename(oracle_value = observation)
  act_forecast <- transform_quantile_model_out(
    model_out_tbl = model_out_tbl_1,
    oracle_output = oracle_output_1
  )

  exp_forecast <- utils::read.csv(test_path("testdata/exp_forecast_2.csv"))
  class(exp_forecast) <- c("forecast_quantile", "forecast", "data.table", "data.frame")
  expect_equal(act_forecast, exp_forecast)
})


test_that("model_out_tbl_1 columns are valid", {
  model_out_tbl_1 <- utils::read.csv(
    test_path("testdata/model_out_tbl_quantile_1.csv")
  ) |>
    dplyr::rename(loc = location, trgt = target, date = target_end_date)

  oracle_output_1 <- utils::read.csv(
    test_path("testdata/target_data_1.csv")
  ) |>
    dplyr::rename(loc = location, date = target_end_date, oracle_value = observation)
  act_forecast <- transform_quantile_model_out(
    model_out_tbl = model_out_tbl_1,
    oracle_output = oracle_output_1
  )
  exp_forecast <- utils::read.csv(
    test_path("testdata/exp_forecast_2.csv")
  ) |>
    dplyr::rename(loc = location, trgt = target, date = target_end_date)
  class(exp_forecast) <- c("forecast_quantile", "forecast", "data.table", "data.frame")
  expect_equal(act_forecast, exp_forecast, ignore_attr = "class")
})

test_that("model_out_tbl_1 has any rows", {
  # Error is thrown by scoringutils::assert_forecast_generic()
  model_out_tbl_1 <- utils::read.csv(
    test_path("testdata/model_out_tbl_quantile_1.csv")
  )
  oracle_output_1 <- utils::read.csv(
    test_path("testdata/target_data_1.csv")
  ) |>
    dplyr::rename(oracle_value = observation)
  expect_error(
    suppressWarnings(transform_quantile_model_out(
      model_out_tbl = model_out_tbl_1[0, ],
      oracle_output = oracle_output_1
    )),
    regexp = "Must have at least 1 rows, but has 0 rows."
  )
})


test_that("many-to-one relationship exists between model_out_tbl and oracle_output", {
  model_out_tbl_1 <- utils::read.csv(
    test_path("testdata/model_out_tbl_quantile_1.csv")
  )
  oracle_output_2 <- utils::read.csv(
    test_path("testdata/target_data_2.csv")
  ) |>
    dplyr::rename(oracle_value = observation)

  expect_error(
    suppressMessages(transform_quantile_model_out(
      model_out_tbl = model_out_tbl_1,
      oracle_output = oracle_output_2
    )),
    regexp = "Each row in `x` must match at most 1 row in `y`."
  )
})

test_that("hubExamples data set is transformed correctly", {
  # forecast_outputs.rda & forecast_oracle_output.rda are stored in hubExamples:
  # https://github.com/hubverse-org/hubExamples/tree/main
  model_out_tbl <- hubex_forecast_outputs()
  oracle_output <- hubex_forecast_oracle_output()
  act_forecast <- transform_quantile_model_out(
    model_out_tbl = model_out_tbl,
    oracle_output = oracle_output
  )

  exp_forecast <- model_out_tbl |>
    dplyr::filter(output_type == "quantile") |>
    dplyr::rename(model = model_id, quantile_level = output_type_id) |>
    dplyr::mutate(quantile_level = as.numeric(quantile_level))
  expect_s3_class(
    act_forecast,
    c("forecast_quantile", "forecast", "data.table", "data.frame")
  )

  # same number of row as quantile predictions from model_out_tbl,
  # same predicted values
  expect_equal(nrow(exp_forecast), nrow(act_forecast))
  exp_act_forecast <- dplyr::full_join(
    exp_forecast, act_forecast,
    by = c("model", "reference_date", "target", "horizon", "location", "target_end_date", "quantile_level")
  )
  expect_equal(exp_act_forecast$predicted, exp_act_forecast$value)

  # correct observed values, in alignment with oracle_output
  exp_act_forecast <- dplyr::left_join(
    act_forecast,
    oracle_output |>
      dplyr::filter(output_type == "quantile") |>
      dplyr::rename(quantile_level = output_type_id) |>
      dplyr::mutate(quantile_level = as.numeric(quantile_level)),
    by = c("target", "location", "target_end_date")
  )
  expect_equal(exp_act_forecast$observed, exp_act_forecast$oracle_value)
})
