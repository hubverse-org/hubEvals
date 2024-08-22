#' Score model output predictions with a single `output_type` against observed data
#'
#' @param model_out_tbl Model output tibble with predictions
#' @param target_observations Observed 'ground truth' data to be compared to
#' predictions
#' @param metrics Optional list of scoring metrics to compute. See details for more.
#' @param summarize boolean indicator of whether summaries of forecast scores
#' should be computed. Defaults to `TRUE`.
#' @param by character vector naming columns to summarize by. For example,
#' specifying `by = "model_id"` will compute average scores for each model. The
#' default, `NULL`, will summarize across `output_type_id` levels (e.g.,
#' quantile levels or categories for a pmf forecast) to produce a single row of
#' scores for each combination of model and prediction task.
#' @param output_type_id_order For ordinal variables in pmf format, this is a
#' vector of levels for pmf forecasts, in increasing order of the levels. For
#' all other output types, this is ignored.
#'
#' @details If `metrics` is `NULL` (the default), this function chooses
#' appropriate metrics based on the `output_type` contained in the `model_out_tbl`:
#' - For `output_type == "quantile"`, we use the default metrics provided by
#' `scoringutils::metrics_quantile()`: "wis", "overprediction", "underprediction",
#' "dispersion", "bias", "interval_coverage_50", "interval_coverage_90",
#' "interval_coverage_deviation", and "ae_median"
#' - For `output_type == "pmf"` and `output_type_id_order` is `NULL` (indicating
#' that the predicted variable is a nominal variable), we use the default metrics
#' provided by `scoringutils::metrics_nominal()`
#' - For `output_type == "median"`, we use "ae_point"
#' - For `output_type == "mean"`, we use "se_point"
#'
#' @return forecast_quantile
#'
#' @export
score_model_out <- function(model_out_tbl, target_observations, metrics = NULL,
                            summarize = TRUE, by = NULL,
                            output_type_id_order = NULL) {
  # check that model_out_tbl has a single output_type that is supported by this package
  # also, retrieve that output_type
  output_type <- validate_output_type(model_out_tbl)

  # get/validate the scoring metrics
  metrics <- get_metrics(metrics, output_type, output_type_id_order)

  # assemble data for scoringutils and select output_type-specific metrics
  if (output_type == "quantile") {
    su_data <- transform_quantile_model_out(model_out_tbl, target_observations)
    if (is.null(metrics)) {
      metrics <- scoringutils::metrics_quantile()
    }
  } else if (output_type == "pmf") {
    su_data <- transform_pmf_model_out(model_out_tbl, target_observations, output_type_id_order)
    if (is.null(metrics)) {
      metrics <- scoringutils::metrics_nominal()
    }
  } else if (output_type %in% c("mean", "median")) {
    su_data <- transform_point_model_out(model_out_tbl, target_observations, output_type)
    if (is.null(metrics)) {
      metrics <- scoringutils::metrics_point(
        select = ifelse(output_type == "mean", "se_point", "ae_point")
      )
    }
  }

  # compute scores
  scores <- scoringutils::score(su_data, metrics)

  # switch back to hubverse convention of "model_id" rather than "model"
  scores <- dplyr::rename(scores, model_id = "model")

  # if requested, summarize scores
  if (summarize) {
    scores <- scoringutils::summarize_scores(scores = scores, by = by)
  }

  return(scores)
}


#' Get metrics if user didn't specify anything; otherwise, process
#' and validate user inputs
#'
#' @inheritParams score_model_out
#'
#' @return a list of metric functions as required by scoringutils::score()
#'
#' @noRd
get_metrics <- function(metrics, output_type, output_type_id_order) {
  if (is.null(metrics)) {
    return(get_metrics_default(output_type, output_type_id_order))
  } else if (is.character(metrics)) {
    return(get_metrics_character(metrics))
  } else {
    # We do a minimal preliminary check that the provided `metrics` is a list of
    # functions, leaving detailed checks to scoringutils
    checkmate::assert_list(metrics, types = "function")
    return(metrics)
  }
}


#' Default metrics if user didn't specify anything
#'
#' @inheritParams score_model_out
#'
#' @return a list of metric functions as required by scoringutils::score()
#'
#' @noRd
get_metrics_default <- function(output_type, output_type_id_order) {
  if (output_type == "quantile") {
    metrics <- scoringutils::metrics_quantile()
  } else if (output_type == "pmf") {
    metrics <- scoringutils::metrics_nominal()
  } else if (output_type == "mean") {
    metrics <- scoringutils::metrics_point(select = "se_point")
  } else if (output_type == "median") {
    metrics <- scoringutils::metrics_point(select = "ae_point")
  } else {
    # we have already validated `output_type`, so this case should not be
    # triggered; this case is just double checking in case we add something new
    # later.
    supported_types <- c("mean", "median", "pmf", "quantile")
    cli::cli_abort(
      "Provided `model_out_tbl` contains `output_type` {.val {output_type}};
      hubEvals currently only supports the following types:
      {.val {supported_types}}"
    )
  }

  return(metrics)
}


#' Convert character vector of metrics to list of functions
#'
#' @inheritParams score_model_out
#'
#' @return a list of metric functions as required by scoringutils::score()
#'
#' @noRd
get_metrics_character <- function(metrics) {
  
}
