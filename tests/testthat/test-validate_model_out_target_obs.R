test_that("validate_model_out_target_obs() throw errors for invalid inputs", {
  expect_error(
    validate_model_out_target_obs(
      model_out_tbl = NULL,
      target_observations = NULL
    ),
    regexp = "model_out_tbl does not contain required columns: model_id, output_type, output_type_id, value"
  )
  expect_error(
    suppressMessages(validate_model_out_target_obs(
      model_out_tbl = data.frame(),
      target_observations = NULL
    )),
    regexp = "model_out_tbl does not contain required columns: model_id, output_type, output_type_id, value"
  )
  expect_error(
    suppressMessages(validate_model_out_target_obs(
      model_out_tbl = data.frame(),
      target_observations = data.frame()
    )),
    regexp = "model_out_tbl does not contain required columns: model_id, output_type, output_type_id, value"
  )
})

test_that("valdiate_model_out_target_obs() works as expected for valid inputs", {
  # all clear
  model_out_tbl_1 <- utils::read.csv(
    test_path("testdata/model_out_tbl_point_1.csv")
  )
  target_observations_1 <- utils::read.csv(
    test_path("testdata/target_data_1.csv")
  )
  val_result <- validate_model_out_target_obs(
    model_out_tbl = model_out_tbl_1,
    target_observations = target_observations_1
  )

  # expected col names in order
  expected_cols <- c("model_id", "location", "reference_date", "horizon",
                     "target_end_date", "target", "model_abbr", "team_abbr",
                     "output_type", "output_type_id", "value")
  expect_equal(
    as.data.frame(val_result),
    model_out_tbl_1[, expected_cols]
  )
  expect_setequal(colnames(model_out_tbl_1), colnames(val_result))
})


test_that("test target_observations has observation column", {
  model_out_tbl_1 <- utils::read.csv(
    test_path("testdata/model_out_tbl_point_1.csv")
  )
  target_observations_1 <- utils::read.csv(
    test_path("testdata/target_data_1.csv")
  ) |>
    dplyr::select(-c("observation"))
  expect_error(
    suppressMessages(validate_model_out_target_obs(
      model_out_tbl = model_out_tbl_1,
      target_observations = target_observations_1
    )),
    regexp = "target_observations does not have observation column"
  )
})


test_that("model_out_tbl columns match target_observations columns", {
  model_out_tbl_1 <- utils::read.csv(
    test_path("testdata/model_out_tbl_point_1.csv")
  )
  target_observations_1 <- utils::read.csv(
    test_path("testdata/target_data_1.csv")
  )

  expect_error(
    suppressMessages(validate_model_out_target_obs(
      model_out_tbl = model_out_tbl_1 |>
        dplyr::rename(loc = location, trgt = target, date = target_end_date),
      target_observations = target_observations_1
    )),
    regexp = "model_out_tbl and target_observations do not have compatible columns"
  )

  cli::test_that_cli("alert", {
    expect_snapshot(
      val_result <- validate_model_out_target_obs(
        model_out_tbl = model_out_tbl_1 |>
          dplyr::rename(loc = location),
        target_observations = target_observations_1
      )
    )
  })
})
