library(tidyverse)

# create model_out_tbl
model_out_tbl <- expand.grid(
  model_id = c("model_A", "model_B"),
  location = c("US", "01"),
  reference_date = as.Date(c("2020-01-14", "2020-01-21")),
  horizon = 0:2,
  output_type = "pmf",
  output_type_id = c("cat", "dog", "bird"),
  stringsAsFactors = FALSE
) |>
  dplyr::mutate(
    target_date = reference_date + 7 * horizon,
    value = row_number()
  ) |>
  dplyr::group_by(model_id, location, reference_date, target_date) |>
  dplyr::mutate(value = value / sum(value)) |>
  dplyr::ungroup()

# check that probs sum to 1
model_out_tbl |>
  group_by(model_id, location, reference_date, target_date) |>
  summarize(tot = sum(value)) |>
  pull(tot)

# create target_obserations
observed_categories <- data.frame(
  location = c("US", "US", "US", "US", "01", "01", "01", "01"),
  target_date = as.Date(c("2020-01-14", "2020-01-21", "2020-01-28", "2020-02-04",
                          "2020-01-14", "2020-01-21", "2020-01-28", "2020-02-04")),
  output_type_id = c("cat", "dog", "cat", "bird", "bird", "dog", "dog", "cat"),
  observation = 1,
  stringsAsFactors = FALSE
)

target_observations <- model_out_tbl |>
  dplyr::distinct(location, target_date, output_type_id) |>
  dplyr::left_join(observed_categories, by = c("location", "target_date", "output_type_id")) |>
  dplyr::mutate(observation = ifelse(is.na(observation), 0, 1))

# check that observations sum to 1
target_observations |>
  group_by(location, target_date) |>
  summarize(tot = sum(observation)) |>
  pull(tot)

# create expected forecast output
exp_forecast <- model_out_tbl |>
  dplyr::mutate(
    predicted_label = factor(output_type_id, levels = c("cat", "dog", "bird"))
  ) |>
  dplyr::select(-output_type, -output_type_id) |>
  dplyr::left_join(
    observed_categories |>
      dplyr::select(-observation) |>
      dplyr::rename(observed = output_type_id),
    by = c("location", "target_date")
  ) |>
  dplyr::mutate(
    observed = factor(observed, levels = c("cat", "dog", "bird"))
  ) |>
  dplyr::select(
    predicted_label, predicted = value, observed, model = model_id,
    location, reference_date, horizon, target_date
  )

class(exp_forecast) <- c("forecast_nominal", "forecast", "data.table", "data.frame")


dput_fun <- function(obj, path = stdout()) {
  x <- get(obj)
  if (!is.function(x)) {
    x <- paste(capture.output(dput(x)), collapse = "\n  ")
    res <- glue::glue("# nolint start\npmf_test_{obj} <- function() {{\n  {dput(x)}\n}}\n# nolint end")
    writeLines(res, path)
  }
}
dput_fun("model_out_tbl", "tests/testthat/helper-pmf_test_model_out_tbl.R")
dput_fun("target_observations", "tests/testthat/helper-pmf_test_target_observations.R")
dput_fun("exp_forecast", "tests/testthat/helper-pmf_test_expected_merged_forecast.R")
