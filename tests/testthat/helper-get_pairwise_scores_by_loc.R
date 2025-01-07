#' Geometric mean
#' (x_1 \times x_2 \times \ldots \times x_n)^{1/n}
#'  = exp[1/n \sum_{i=1}^{n} log(x_i)]
geometric_mean <- function(x) {
  exp(mean(log(x)))
}


#' Helper function manually computes pairwise relative skill scores by location.
#' Called from tests in test-score_model_out_rel_metrics.R
get_pairwise_scores_by_loc <- function(scores_per_task, metric, baseline = NULL) {
  mean_scores_by_loc <- scores_per_task |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("model_id", "location")))) |>
    dplyr::summarize(
      mean_score = mean(.data[[metric]], na.rm = TRUE),
      .groups = "drop"
    )

  pairwise_score_ratios <- expand.grid(
    model_id = unique(mean_scores_by_loc$model_id),
    model_id_compare = unique(mean_scores_by_loc$model_id),
    location = unique(mean_scores_by_loc[["location"]])
  ) |>
    dplyr::left_join(mean_scores_by_loc, by = c("model_id" = "model_id", "location")) |>
    dplyr::left_join(mean_scores_by_loc, by = c("model_id_compare" = "model_id", "location")) |>
    dplyr::mutate(
      pairwise_score_ratio = mean_score.x / mean_score.y # nolint: object_usage_linter
    )

  result <- pairwise_score_ratios |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("model_id", "location")))) |>
    dplyr::summarize(
      relative_skill = geometric_mean(pairwise_score_ratio), # nolint: object_usage_linter
      .groups = "drop"
    )
  out_colnames <- c("model_id", "location", paste0(metric, "_relative_skill"))

  if (!is.null(baseline)) {
    result <- result |>
      dplyr::group_by(location) |> # nolint: object_usage_linter
      dplyr::mutate(
        scaled_relative_skill = relative_skill / relative_skill[model_id == baseline] # nolint: object_usage_linter
      )
    out_colnames <- c(out_colnames, paste0(metric, "_scaled_relative_skill"))
  }

  colnames(result) <- out_colnames

  return(result)
}
