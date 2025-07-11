#' Score model output predictions
#'
#' Scores model outputs with a single `output_type` against observed data.
#'
#' @param model_out_tbl Model output tibble with predictions
#' @param oracle_output Predictions that would have been generated by an oracle
#' model that knew the observed target data values in advance
#' @param metrics Character vector of scoring metrics to compute. If `NULL`
#' (the default), appropriate metrics are chosen automatically. See details
#' for more.
#' @param relative_metrics Character vector of scoring metrics for which to
#' compute relative skill scores. The `relative_metrics` should be a subset of
#' `metrics` and should only include proper scores (e.g., it should not contain
#' interval coverage metrics).  If `NULL` (the default), no relative metrics
#' will be computed.  Relative metrics are only computed if `summarize = TRUE`,
#' and require that `"model_id"` is included in `by`.
#' @param baseline String with the name of a model to use as a baseline for
#' relative skill scores. If a baseline is given, then a scaled relative skill
#' with respect to the baseline will be returned. By default (`NULL`), relative
#' skill will not be scaled with respect to a baseline model.
#' @param summarize Boolean indicator of whether summaries of forecast scores
#' should be computed. Defaults to `TRUE`.
#' @param by Character vector naming columns to summarize by. For example,
#' specifying `by = "model_id"` (the default) will compute average scores for
#' each model.
#' @param output_type_id_order For ordinal variables in pmf format, this is a
#' vector of levels for pmf forecasts, in increasing order of the levels. The
#' order of the values for the output_type_id can be found by referencing the
#' hub's tasks.json configuration file. For all output types other than pmf,
#' this is ignored.
#'
#' @details
#' See the hubverse documentation for the expected format of the
#' [oracle output data](https://docs.hubverse.io/en/latest/user-guide/target-data.html#oracle-output).
#'
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
#' See [scoringutils::get_metrics.forecast_nominal] for details.
#'
#' **Ordinal forecasts:** (`output_type == "pmf"` and `output_type_id_order` is a vector)
#'
#' `r paste("- ", names(scoringutils::get_metrics(scoringutils::example_ordinal)), collapse = "\n")`
#'
#' See [scoringutils::get_metrics.forecast_ordinal] for details.
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
#' See [scoringutils::add_relative_skill] for details on relative skill scores.
#'
#' @examplesIf requireNamespace("hubExamples", quietly = TRUE)
#' # compute WIS and interval coverage rates at 80% and 90% levels based on
#' # quantile forecasts, summarized by the mean score for each model
#' quantile_scores <- score_model_out(
#'   model_out_tbl = hubExamples::forecast_outputs |>
#'     dplyr::filter(.data[["output_type"]] == "quantile"),
#'   oracle_output = hubExamples::forecast_oracle_output,
#'   metrics = c("wis", "interval_coverage_80", "interval_coverage_90"),
#'   relative_metrics = "wis",
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
#'   oracle_output = hubExamples::forecast_oracle_output,
#'   metrics = c("log_score", "rps"),
#'   by = c("model_id", "location", "horizon"),
#'   output_type_id_order = c("low", "moderate", "high", "very high")
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
score_model_out <- function(model_out_tbl, oracle_output,
                            metrics = NULL, relative_metrics = NULL, baseline = NULL,
                            summarize = TRUE, by = "model_id",
                            output_type_id_order = NULL) {
  # check that model_out_tbl has a single output_type that is supported by this package
  # also, retrieve that output_type
  output_type <- validate_output_type(model_out_tbl)
  if (summarize) {
    # Note: The call to scoringutils::add_relative_skill below performs validation of `baseline`
    validate_relative_metrics(relative_metrics, metrics, by)
  }

  # assemble data for scoringutils
  su_data <- switch(output_type,
    quantile = transform_quantile_model_out(model_out_tbl, oracle_output),
    pmf = transform_pmf_model_out(model_out_tbl, oracle_output, output_type_id_order),
    mean = transform_point_model_out(model_out_tbl, oracle_output, output_type),
    median = transform_point_model_out(model_out_tbl, oracle_output, output_type),
    NULL # default, should not happen because of the validation above
  )

  # get/validate the scoring metrics
  metrics <- get_metrics(forecast = su_data, output_type = output_type, select = metrics)

  # compute scores
  scores <- scoringutils::score(su_data, metrics)

  # switch back to hubverse naming conventions for model name
  scores <- dplyr::rename(scores, model_id = "model")

  # if requested, summarize scores, including computation of relative metrics
  if (summarize) {
    for (metric in relative_metrics) {
      scores <- scoringutils::add_relative_skill(
        scores,
        compare = "model_id",
        by = by[by != "model_id"],
        metric = metric,
        baseline = baseline
      )
    }
    scores <- scoringutils::summarize_scores(scores = scores, by = by)
  }

  scores
}


#' Get scoring metrics
#'
#' @param forecast A scoringutils `forecast` object (see
#' [scoringutils' general information on creating a forecast object][scoringutils::as_forecast_doc_template()]
#' for details).
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
    interval_metric_inds <- is_interval_coverage_metric(select)
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
        purrr::partial(scoringutils::interval_coverage, interval_range = level)
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

  metric_fns
}


#' Determine whether metric is an interval coverage metric
#'
#' @param metrics Character vector of metric names
#' @return Logical vector indicating whether each metric is an interval coverage metric
#' @noRd
is_interval_coverage_metric <- function(metric) {
  grepl(pattern = "^interval_coverage_", metric)
}
