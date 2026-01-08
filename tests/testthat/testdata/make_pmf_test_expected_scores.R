# Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
forecast_outputs <- hubExamples::forecast_outputs
forecast_oracle_output <- hubExamples::forecast_oracle_output

# expected scores
## log scores
exp_log_scores <- forecast_outputs |>
  dplyr::filter(.data[["output_type"]] == "pmf") |>
  dplyr::left_join(
    forecast_oracle_output |>
      dplyr::filter(.data[["output_type"]] == "pmf") |>
      dplyr::select(-dplyr::all_of(c("output_type"))),
    by = c("location", "target_end_date", "target", "output_type_id")
  ) |>
  dplyr::filter(.data[["oracle_value"]] == 1) |>
  dplyr::mutate(
    log_score = -1 * log(.data[["value"]])
  ) |>
  dplyr::group_by(dplyr::across(dplyr::all_of(
    c("model_id", "location")
  ))) |>
  dplyr::summarize(
    log_score = mean(.data[["log_score"]]),
    .groups = "drop"
  )

## rps scores
## see Eq (1) of Weigen et al. (2006) The Discrete Brier and Ranked Probability Skill Scores.
## Monthly Weather Review, 135, 118-124.
exp_rps_scores <- forecast_outputs |>
  dplyr::filter(.data[["output_type"]] == "pmf") |>
  dplyr::left_join(
    forecast_oracle_output |>
      dplyr::filter(.data[["output_type"]] == "pmf") |>
      dplyr::select(-dplyr::all_of(c("output_type"))),
    by = c("location", "target_end_date", "target", "output_type_id")
  ) |>
  dplyr::group_by(dplyr::across(dplyr::all_of(
    c(
      "model_id",
      "location",
      "reference_date",
      "horizon",
      "target_end_date",
      "target"
    )
  ))) |>
  dplyr::summarize(
    rps = sum((cumsum(.data[["value"]]) - cumsum(.data[["oracle_value"]]))^2)
  ) |>
  dplyr::group_by(dplyr::across(dplyr::all_of(
    c("model_id", "location")
  ))) |>
  dplyr::summarize(
    rps = mean(.data[["rps"]]),
    .groups = "drop"
  )

exp_scores <- dplyr::full_join(
  exp_log_scores,
  exp_rps_scores,
  by = c("model_id", "location")
)

write.csv(
  exp_scores,
  testthat::test_path("testdata", "exp_pmf_scores.csv"),
  row.names = FALSE
)
