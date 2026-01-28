#' Geometric mean
#' (x_1 \times x_2 \times \ldots \times x_n)^{1/n}
#'  = exp[1/n \sum_{i=1}^{n} log(x_i)]
geometric_mean <- function(x) {
  exp(mean(log(x)))
}


#' Helper function manually computes pairwise relative skill scores by location
#' Called from tests in test-score_model_out_rel_metrics.R
get_pairwise_scores_by_loc <- function(scores_per_task, metric, baseline) {
  mean_scores_by_loc <- scores_per_task |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("model_id", "location")))) |>
    dplyr::summarize(
      mean_score = mean(.data[[metric]], na.rm = TRUE), # nolint: object_usage_linter
      .groups = "drop"
    )

  pairwise_score_ratios <- expand.grid(
    model_id = unique(mean_scores_by_loc$model_id),
    model_id_compare = unique(mean_scores_by_loc$model_id),
    location = unique(mean_scores_by_loc[["location"]])
  ) |>
    dplyr::left_join(
      mean_scores_by_loc,
      by = c("model_id" = "model_id", "location")
    ) |>
    dplyr::left_join(
      mean_scores_by_loc,
      by = c("model_id_compare" = "model_id", "location")
    ) |>
    dplyr::mutate(
      pairwise_score_ratio = .data[["mean_score.x"]] / .data[["mean_score.y"]] # nolint: object_usage_linter
    )

  result <- pairwise_score_ratios |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("model_id", "location")))) |>
    dplyr::summarize(
      relative_skill = geometric_mean(.data[["pairwise_score_ratio"]]), # nolint: object_usage_linter
      .groups = "drop"
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of("location"))) |>
    dplyr::mutate(
      scaled_relative_skill = .data[["relative_skill"]] /
        .data[["relative_skill"]][.data[["model_id"]] == baseline]
    )

  colnames(result) <- c(
    "model_id",
    "location",
    paste0(metric, "_relative_skill"),
    paste0(metric, "_scaled_relative_skill")
  )

  result
}


# Forecast data from hubExamples: <https://hubverse-org.github.io/hubExamples/reference/forecast_data.html>
forecast_outputs <- hubExamples::forecast_outputs
forecast_oracle_output <- hubExamples::forecast_oracle_output

# expected scores
exp_scores_unsummarized <- forecast_outputs |>
  dplyr::filter(.data[["output_type"]] == "quantile") |>
  dplyr::left_join(
    forecast_oracle_output |>
      dplyr::filter(.data[["output_type"]] == "quantile") |>
      dplyr::select(-dplyr::all_of(c("output_type", "output_type_id"))),
    by = c("location", "target_end_date", "target")
  ) |>
  dplyr::mutate(
    output_type_id = as.numeric(.data[["output_type_id"]]),
    qs = ifelse(
      .data[["oracle_value"]] >= .data[["value"]],
      .data[["output_type_id"]] * (.data[["oracle_value"]] - .data[["value"]]),
      (1 - .data[["output_type_id"]]) *
        (.data[["value"]] - .data[["oracle_value"]])
    ),
    q_coverage_80_lower = ifelse(
      .data[["output_type_id"]] == 0.1,
      .data[["oracle_value"]] >= .data[["value"]],
      NA_real_
    ),
    q_coverage_80_upper = ifelse(
      .data[["output_type_id"]] == 0.9,
      .data[["oracle_value"]] <= .data[["value"]],
      NA_real_
    ),
    q_coverage_90_lower = ifelse(
      .data[["output_type_id"]] == 0.05,
      .data[["oracle_value"]] >= .data[["value"]],
      NA_real_
    ),
    q_coverage_90_upper = ifelse(
      .data[["output_type_id"]] == 0.95,
      .data[["oracle_value"]] <= .data[["value"]],
      NA_real_
    )
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
    ae_median = sum(ifelse(
      .data[["output_type_id"]] == 0.5,
      abs(.data[["oracle_value"]] - .data[["value"]]),
      0
    )),
    wis = 2 * mean(.data[["qs"]]),
    interval_coverage_80 = (sum(.data[["q_coverage_80_lower"]],
                                na.rm = TRUE) == 1) *
      (sum(.data[["q_coverage_80_upper"]], na.rm = TRUE) == 1),
    interval_coverage_90 = (sum(.data[["q_coverage_90_lower"]],
                                na.rm = TRUE) == 1) *
      (sum(.data[["q_coverage_90_upper"]], na.rm = TRUE) == 1)
  )

exp_scores_standard <- exp_scores_unsummarized |>
  dplyr::group_by(dplyr::across(dplyr::all_of(
    c("model_id", "location")
  ))) |>
  dplyr::summarize(
    ae_median = mean(.data[["ae_median"]]),
    wis = mean(.data[["wis"]]),
    interval_coverage_80 = mean(.data[["interval_coverage_80"]], na.rm = TRUE),
    interval_coverage_90 = mean(.data[["interval_coverage_90"]], na.rm = TRUE),
    .groups = "drop"
  )

# add pairwise relative scores for ae_median and wis
exp_scores_relative_ae_median <- get_pairwise_scores_by_loc(
  exp_scores_unsummarized,
  "ae_median",
  "Flusight-baseline"
)
exp_scores_relative_wis <- get_pairwise_scores_by_loc(
  exp_scores_unsummarized,
  "wis",
  "Flusight-baseline"
)
exp_scores <- exp_scores_standard |>
  dplyr::full_join(
    exp_scores_relative_ae_median,
    by = c("model_id", "location")
  ) |>
  dplyr::full_join(exp_scores_relative_wis, by = c("model_id", "location"))

# save
write.csv(
  exp_scores,
  testthat::test_path("testdata", "exp_pairwise_scores.csv"),
  row.names = FALSE
)
