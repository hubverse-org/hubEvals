test_that("validate_model_oracle_out() throw errors for invalid inputs", {
  expect_error(
    validate_model_oracle_out(
      model_out_tbl = NULL,
      oracle_output = NULL
    ),
    regexp = "model_out_tbl does not contain required columns: model_id, output_type, output_type_id, value"
  )
  expect_error(
    suppressMessages(validate_model_oracle_out(
      model_out_tbl = data.frame(),
      oracle_output = NULL
    )),
    regexp = "model_out_tbl does not contain required columns: model_id, output_type, output_type_id, value"
  )
  expect_error(
    suppressMessages(validate_model_oracle_out(
      model_out_tbl = data.frame(),
      oracle_output = data.frame()
    )),
    regexp = "model_out_tbl does not contain required columns: model_id, output_type, output_type_id, value"
  )
})

test_that("valdiate_model_out_target_obs() works as expected for valid inputs", {
  # all clear
  model_out_tbl_1 <- utils::read.csv(
    test_path("testdata/model_out_tbl_point_1.csv")
  )
  oracle_output_1 <- utils::read.csv(
    test_path("testdata/target_data_1.csv")
  ) |>
    dplyr::rename(oracle_value = observation)
  val_result <- validate_model_oracle_out(
    model_out_tbl = model_out_tbl_1,
    oracle_output = oracle_output_1
  )

  # expected col names in order
  expected_cols <- c(
    "model_id",
    "location",
    "reference_date",
    "horizon",
    "target_end_date",
    "target",
    "model_abbr",
    "team_abbr",
    "output_type",
    "output_type_id",
    "value"
  )
  expect_equal(
    as.data.frame(val_result),
    model_out_tbl_1[, expected_cols]
  )
  expect_setequal(colnames(model_out_tbl_1), colnames(val_result))
})


test_that("an error is thrown if oracle_output is missing oracle_value column", {
  model_out_tbl_1 <- utils::read.csv(
    test_path("testdata/model_out_tbl_point_1.csv")
  )
  oracle_output_1 <- utils::read.csv(
    test_path("testdata/target_data_1.csv")
  ) |>
    dplyr::select(-c("observation"))
  expect_error(
    suppressMessages(validate_model_oracle_out(
      model_out_tbl = model_out_tbl_1,
      oracle_output = oracle_output_1
    )),
    regexp = "oracle_output does not have oracle_value column"
  )
})


test_that("an error is thrown if model_out_tbl columns do not match oracle_output columns", {
  model_out_tbl_1 <- utils::read.csv(
    test_path("testdata/model_out_tbl_point_1.csv")
  )
  oracle_output_1 <- utils::read.csv(
    test_path("testdata/target_data_1.csv")
  ) |>
    dplyr::rename(oracle_value = observation)

  expect_error(
    suppressMessages(validate_model_oracle_out(
      model_out_tbl = model_out_tbl_1 |>
        dplyr::rename(loc = location, trgt = target, date = target_end_date),
      oracle_output = oracle_output_1
    )),
    regexp = "model_out_tbl and oracle_output do not have compatible columns"
  )
})


test_that("validate_model_oracle_out() throws error for unexpected columns", {
  model_out_tbl_1 <- utils::read.csv(
    test_path("testdata/model_out_tbl_point_1.csv")
  )
  oracle_output_1 <- utils::read.csv(
    test_path("testdata/target_data_1.csv")
  ) |>
    dplyr::rename(oracle_value = observation)
  expect_error(
    suppressMessages(validate_model_oracle_out(
      model_out_tbl = model_out_tbl_1 |>
        dplyr::rename(loc = location),
      oracle_output = oracle_output_1
    )),
    regexp = "unexpected column"
  )
})
