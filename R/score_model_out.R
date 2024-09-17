#' Score model output predictions
#'
#' Scores model outputs with a single `output_type` against observed data.
#'
#' @param model_out_tbl Model output tibble with predictions
#' @param target_observations Observed 'ground truth' data to be compared to
#' predictions
#' @param metrics Character vector of scoring metrics to compute. If `NULL`
#' (the default), appropriate metrics are chosen automatically. See details
#' for more.
#' @param summarize Boolean indicator of whether summaries of forecast scores
#' should be computed. Defaults to `TRUE`.
#' @param by Character vector naming columns to summarize by. For example,
#' specifying `by = "model_id"` (the default) will compute average scores for
#' each model.
#' @param output_type_id_order For ordinal variables in pmf format, this is a
#' vector of levels for pmf forecasts, in increasing order of the levels. For
#' all other output types, this is ignored.
#'
#' @details
#' Default metrics are provided by the `scoringutils` package. You can select
#' metrics by passing in a character vector of metric names to the `metrics`
#' argument.
#'
#' The following metrics can be selected (all are used by default) for the
#' different `output_type`s:
#'
#' **Quantile forecasts:** (`output_type == "quantile"`)
#' `r exclude <- c("interval_coverage_50", "interval_coverage_90")`
#' `r metrics <- scoringutils::get_metrics(scoringutils::example_quantile, exclude = exclude)`
#' `r paste("- ", names(metrics), collapse = "\n")`
#' - "interval_coverage_XX": interval coverage at the "XX" level. For example,
#' "interval_coverage_95" is the 95% interval coverage rate, which would be calculated
#' based on quantiles at the probability levels 0.025 and 0.975.
#'
#' See [scoringutils::get_metrics.forecast_quantile] for details.
#'
#' **Nominal forecasts:** (`output_type == "pmf"` and `output_type_id_order` is `NULL`)
#'
#' `r paste("- ", names(scoringutils::get_metrics(scoringutils::example_nominal)), collapse = "\n")`
#'
#' (scoring for ordinal forecasts will be added in the future).
#'
#' See [scoringutils::get_metrics.forecast_nominal] for details.
#'
#' **Median forecasts:** (`output_type == "median"`)
#'
#' - ae_point: absolute error of the point forecast (recommended for the median, see Gneiting (2011))
#'
#' See [scoringutils::get_metrics.forecast_point] for details.
#'
#' **Mean forecasts:** (`output_type == "mean"`)
#' - `se_point`: squared error of the point forecast (recommended for the mean, see Gneiting (2011))
#'
#' @examplesIf requireNamespace("hubExamples", quietly = TRUE)
#' # compute WIS and interval coverage rates at 80% and 90% levels based on
#' # quantile forecasts, summarized by the mean score for each model
#' quantile_scores <- score_model_out(
#'   model_out_tbl = hubExamples::forecast_outputs |>
#'     dplyr::filter(.data[["output_type"]] == "quantile"),
#'   target_observations = hubExamples::forecast_target_observations,
#'   metrics = c("wis", "interval_coverage_80", "interval_coverage_90"),
#'   by = "model_id"
#' )
#' quantile_scores
#'
#' # compute log scores based on pmf predictions for categorical targets,
#' # summarized by the mean score for each combination of model and location.
#' # Note: if the model_out_tbl had forecasts for multiple targets using a
#' # pmf output_type with different bins, it would be necessary to score the
#' # predictions for those targets separately.
#' pmf_scores <- score_model_out(
#'   model_out_tbl = hubExamples::forecast_outputs |>
#'     dplyr::filter(.data[["output_type"]] == "pmf"),
#'   target_observations = hubExamples::forecast_target_observations,
#'   metrics = "log_score",
#'   by = c("model_id", "location", "horizon")
#' )
#' head(pmf_scores)
#'
#' @return A data.table with scores
#'
#' @references
#' Gneiting, Tilmann. 2011. "Making and Evaluating Point Forecasts." Journal of the 
#' American Statistical Association 106 (494): 746–62. <doi: 10.1198/jasa.2011.r10138>.
#'
#' @export
score_model_out <- function(model_out_tbl, target_observations, metrics = NULL,
                            summarize = TRUE, by = "model_id",
                            output_type_id_order = NULL) {
  # check that model_out_tbl has a single output_type that is supported by this package
  # also, retrieve that output_type
  output_type <- validate_output_type(model_out_tbl)

  # assemble data for scoringutils
  su_data <- switch(output_type,
    quantile = transform_quantile_model_out(model_out_tbl, target_observations),
    pmf = transform_pmf_model_out(model_out_tbl, target_observations, output_type_id_order),
    mean = transform_point_model_out(model_out_tbl, target_observations, output_type),
    median = transform_point_model_out(model_out_tbl, target_observations, output_type),
    NULL # default, should not happen because of the validation above
  )

  # get/validate the scoring metrics
  metrics <- get_metrics(forecast = su_data, output_type = output_type, select = metrics)

  # compute scores
  scores <- scoringutils::score(su_data, metrics)

  # switch back to hubverse naming conventions for model name
  scores <- dplyr::rename(scores, model_id = "model")

  # if requested, summarize scores
  if (summarize) {
    scores <- scoringutils::summarize_scores(scores = scores, by = by)
  }

  return(scores)
}


#' Get scoring metrics
#'
#' @param forecast A scoringutils `forecast` object (see
#' [scoringutils::as_forecast()] for details).
#' @inheritParams score_model_out
#'
#' @return a list of metric functions as required by scoringutils::score()
#'
#' @noRd
get_metrics <- function(forecast, output_type, select = NULL) {
  forecast_type <- class(forecast)[1]

  # process quantile metrics separately to allow better selection of interval
  # coverage metrics
  if (forecast_type == "forecast_quantile") {
    # split into metrics for interval coverage and others
    interval_metric_inds <- grepl(pattern = "^interval_coverage_", select)
    interval_metrics <- select[interval_metric_inds]
    other_metrics <- select[!interval_metric_inds]

    other_metric_fns <- scoringutils::get_metrics(forecast, select = other_metrics)

    # assemble interval coverage functions
    interval_metric_fns <- lapply(
      interval_metrics,
      function(metric) {
        level_str <- substr(metric, 19, nchar(metric))
        level <- suppressWarnings(as.numeric(level_str))
        if (is.na(level) || level <= 0 || level >= 100) {
          cli::cli_abort(c(
            "Invalid interval coverage level: {level_str}",
            "i" = "must be a number between 0 and 100 (exclusive)"
          ))
        }
        return(purrr::partial(scoringutils::interval_coverage, interval_range = level))
      }
    )
    names(interval_metric_fns) <- interval_metrics

    metric_fns <- c(other_metric_fns, interval_metric_fns)
    return(metric_fns)
  }

  # leave validation of user selection to scoringutils
  metric_fns <- scoringutils::get_metrics(forecast, select = select)
  if (output_type == "mean") {
    metric_fns <- scoringutils::select_metrics(metric_fns, "se_point")
  } else if (output_type == "median") {
    metric_fns <- scoringutils::select_metrics(metric_fns, "ae_point")
  }

  return(metric_fns)
}
