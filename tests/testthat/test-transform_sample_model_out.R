# --- Marginal sample scoring ---

test_that("hubExamples sample data transforms to forecast_sample for marginal scoring", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  sample_tbl <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "sample")

  act_forecast <- transform_sample_model_out(
    model_out_tbl = sample_tbl,
    oracle_output = forecast_oracle_output
  )

  expect_s3_class(
    act_forecast,
    c("forecast_sample", "forecast", "data.table", "data.frame")
  )

  # same number of rows as sample predictions
  expect_equal(nrow(act_forecast), nrow(sample_tbl))

  # correct column mapping
  expect_named(
    act_forecast,
    c(
      "sample_id",
      "predicted",
      "observed",
      "model",
      "reference_date",
      "target",
      "horizon",
      "location",
      "target_end_date"
    )
  )

  # predicted values match original values. We join rather than compare
  # positionally because as_forecast_sample() converts to data.table which
  # may reorder rows.
  merged <- dplyr::inner_join(
    act_forecast,
    sample_tbl,
    by = dplyr::join_by(
      model == model_id,
      sample_id == output_type_id,
      reference_date,
      target,
      horizon,
      location,
      target_end_date
    ),
    relationship = "one-to-one"
  )
  expect_equal(merged$predicted, merged$value)

  # observed values correctly merged from oracle output
  oracle_sample <- forecast_oracle_output |>
    dplyr::filter(.data[["output_type"]] == "sample") |>
    dplyr::select(-c("output_type", "output_type_id"))
  obs_merged <- dplyr::inner_join(
    act_forecast,
    oracle_sample,
    by = dplyr::join_by(location, target_end_date, target),
    relationship = "many-to-one"
  )
  expect_equal(obs_merged$observed, obs_merged$oracle_value)
})


test_that("oracle output without output_type/output_type_id columns works", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  # Strip output_type and output_type_id from oracle
  oracle_simple <- forecast_oracle_output |>
    dplyr::filter(.data[["output_type"]] == "sample") |>
    dplyr::select(-c("output_type", "output_type_id"))

  sample_tbl <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "sample")

  act_forecast <- transform_sample_model_out(
    model_out_tbl = sample_tbl,
    oracle_output = oracle_simple
  )

  expect_s3_class(act_forecast, "forecast_sample")
  expect_equal(nrow(act_forecast), nrow(sample_tbl))

  # observed values correctly merged from oracle output
  merged <- dplyr::inner_join(
    act_forecast,
    oracle_simple,
    by = dplyr::join_by(location, target_end_date, target),
    relationship = "many-to-one"
  )
  expect_equal(merged$observed, merged$oracle_value)
})


test_that("transform_sample_model_out errors on empty data", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  sample_tbl <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "sample")

  expect_error(
    suppressWarnings(transform_sample_model_out(
      model_out_tbl = sample_tbl[0, ],
      oracle_output = forecast_oracle_output
    )),
    regexp = "Must have at least 1 rows, but has 0 rows."
  )
})


test_that("transform_sample_model_out errors on missing required columns", {
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  bad_tbl <- data.frame(
    model_id = "A",
    output_type = "sample",
    value = 1
  )

  expect_error(
    transform_sample_model_out(
      model_out_tbl = bad_tbl,
      oracle_output = forecast_oracle_output
    ),
    regexp = "does not contain required columns"
  )
})


# --- Compound sample scoring ---

test_that("compound_taskid_set produces forecast_sample_multivariate", {
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  sample_tbl <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "sample")

  act_forecast <- transform_sample_model_out(
    model_out_tbl = sample_tbl,
    oracle_output = forecast_oracle_output,
    compound_taskid_set = c("reference_date", "location")
  )

  expect_s3_class(
    act_forecast,
    c("forecast_sample_multivariate", "forecast", "data.table", "data.frame")
  )
  expect_equal(nrow(act_forecast), nrow(sample_tbl))
  expect_named(
    act_forecast,
    c(
      "sample_id",
      "predicted",
      "observed",
      "model",
      "reference_date",
      "target",
      "horizon",
      "location",
      "target_end_date",
      ".mv_group_id"
    )
  )
})


test_that("validate_compound_taskid_set errors on non-character input", {
  expect_error(
    validate_compound_taskid_set(
      data = data.frame(),
      compound_taskid_set = 123,
      task_id_cols = c("location", "horizon")
    ),
    regexp = "must be a non-empty character vector"
  )
})


test_that("validate_compound_taskid_set errors on empty vector", {
  expect_error(
    validate_compound_taskid_set(
      data = data.frame(),
      compound_taskid_set = character(0),
      task_id_cols = c("location", "horizon")
    ),
    regexp = "must be a non-empty character vector"
  )
})


test_that("validate_compound_taskid_set errors on invalid column names", {
  expect_error(
    validate_compound_taskid_set(
      data = data.frame(),
      compound_taskid_set = c("location", "nonexistent"),
      task_id_cols = c("location", "horizon")
    ),
    regexp = "contains invalid column"
  )
})


test_that("validate_compound_taskid_set errors when all task IDs included", {
  expect_error(
    validate_compound_taskid_set(
      data = data.frame(),
      compound_taskid_set = c("location", "horizon"),
      task_id_cols = c("location", "horizon")
    ),
    regexp = "includes all task ID columns"
  )
})


test_that("validate_compound_taskid_set errors when joint_across dims don't vary", {
  # Create data where non-compound task IDs (horizon) don't vary within draws.
  # A draw is identified by compound_taskid_set + output_type_id.
  data <- data.frame(
    output_type_id = c("s1", "s2"),
    location = c("25", "25"),
    horizon = c(0, 0)
  )

  expect_error(
    validate_compound_taskid_set(
      data = data,
      compound_taskid_set = "location",
      task_id_cols = c("location", "horizon")
    ),
    regexp = "must vary within sample draws"
  )
})


test_that("finer compound_taskid_set with unique IDs errors", {
  # Marginal data with globally unique sample IDs: each sample covers only
  # one (location, horizon) combination. When IDs are unique, joint_across
  # dimensions don't vary within draws, so validation correctly errors.
  data <- data.frame(
    output_type_id = as.character(1:12),
    location = rep(c("US", "UK"), each = 6),
    horizon = rep(rep(1:3, each = 2), 2)
  )

  expect_error(
    validate_compound_taskid_set(
      data = data,
      compound_taskid_set = "location",
      task_id_cols = c("location", "horizon")
    ),
    regexp = "must vary within sample draws"
  )
})


test_that("coarser compound_taskid_set than actual structure passes", {
  # The hubExamples data has compound_taskid_set = {reference_date, location}
  # per tasks.json: each draw has fixed ref_date + location, varying over
  # horizon. Scoring with just {reference_date} is coarser -- it treats
  # location as a joint_across dimension. This passes validation because
  # location does vary within draws (due to sample ID reuse across locations).
  forecast_outputs <- hubExamples::forecast_outputs
  forecast_oracle_output <- hubExamples::forecast_oracle_output

  sample_tbl <- forecast_outputs |>
    dplyr::filter(.data[["output_type"]] == "sample")

  forecast <- transform_sample_model_out(
    model_out_tbl = sample_tbl,
    oracle_output = forecast_oracle_output,
    compound_taskid_set = "reference_date"
  )

  expect_s3_class(forecast, "forecast_multivariate_sample")
  expect_equal(nrow(forecast), nrow(sample_tbl))

  # The same sample_id appears in multiple .mv_group_ids because the actual
  # draw structure is finer than the specified compound_taskid_set -- each
  # draw is being split across locations.
  sample_groups <- unique(as.data.frame(forecast)[, c(
    "sample_id",
    ".mv_group_id"
  )])
  ids_in_multiple <- sample_groups$sample_id[duplicated(
    sample_groups$sample_id
  )]
  expect_true(length(ids_in_multiple) > 0)
})
