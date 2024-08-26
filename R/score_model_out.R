#' Score model output predictions with a single `output_type` against observed data
#'
#' @param model_out_tbl Model output tibble with predictions
#' @param target_observations Observed 'ground truth' data to be compared to
#' predictions
#' @param metrics Optional list of scoring metrics to compute. See details for more.
#' @param summarize boolean indicator of whether summaries of forecast scores
#' should be computed. Defaults to `TRUE`.
#' @param by character vector naming columns to summarize by. For example,
#' specifying `by = "model_id"` (the default) will compute average scores for
#' each model.
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
#' provided by `scoringutils::metrics_nominal()`, currently just "log_score"
#' - For `output_type == "median"`, we use "ae_point"
#' - For `output_type == "mean"`, we use "se_point"
#'
#' It is also possible to directly provide a list of metrics, e.g. as would be
#' created by one of those function calls.  Alternatively, a character vector of
#' scoring metrics can be provided. In this case, the following options are supported:
#' - `output_type == "median"` and `output_type == "median"`:
#'     - "ae": absolute error of a point prediction (generally recommended for the median)
#'     - "se": squared error of a point prediction (generally recommended for the mean)
#' - `output_type == "quantile"`:
#'     - "ae_median": absolute error of the predictive median (i.e., the quantile at probability level 0.5)
#'     - "wis": weighted interval score (WIS) of a collection of quantile predictions
#'     - "overprediction": The component of WIS measuring the extent to which
#' predictions fell above the observation.
#'     - "underprediction": The component of WIS measuring the extent to which
#' predictions fell below the observation.
#'     - "dispersion":  The component of WIS measuring the dispersion of forecast
#' distributions.
#'     - "interval_coverage_XX": interval coverage at the "XX" level. For example,
#' "interval_coverage_95" is the 95% interval coverage rate, which would be calculated
#' based on quantiles at the probability levels 0.025 and 0.975.
#' - `output_type == "pmf"`:
#'     - "log_score": log score
#'
#' @return forecast_quantile
#'
#' @export
score_model_out <- function(model_out_tbl, target_observations, metrics = NULL,
                            summarize = TRUE, by = "model_id",
                            output_type_id_order = NULL) {
  # check that model_out_tbl has a single output_type that is supported by this package
  # also, retrieve that output_type
  output_type <- validate_output_type(model_out_tbl)

  # get/validate the scoring metrics
  metrics <- get_metrics(metrics, output_type, output_type_id_order)

  # assemble data for scoringutils and select output_type-specific metrics
  su_data <- switch(output_type,
    quantile = transform_quantile_model_out(model_out_tbl, target_observations),
    pmf = transform_pmf_model_out(model_out_tbl, target_observations, output_type_id_order),
    mean = transform_point_model_out(model_out_tbl, target_observations, output_type),
    median = transform_point_model_out(model_out_tbl, target_observations, output_type),
    NULL # default, should not happen because of the validation above
  )

  # compute scores
  scores <- scoringutils::score(su_data, metrics)

  # switch back to hubverse naming conventions for model name
  scores <- dplyr::rename(
    scores,
    model_id = "model"
  )

  # if present, drop predicted and observed columns
  drop_cols <- c("predicted", "observed")
  scores <- scores[!colnames(scores) %in% drop_cols]

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
    return(get_metrics_character(metrics, output_type))
  } else {
    # We do a minimal preliminary check that the provided `metrics` is a list of
    # functions, leaving further checks to scoringutils
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
  metrics <- switch(output_type,
    quantile = scoringutils::metrics_quantile(),
    pmf = scoringutils::metrics_nominal(),
    mean = scoringutils::metrics_point(select = "se_point"),
    median = scoringutils::metrics_point(select = "ae_point"),
    NULL # default
  )
  if (is.null(metrics)) {
    # we have already validated `output_type`, so this case should not be
    # triggered; this case is just double checking in case we add something new
    # later, to ensure we update this function.
    supported_types <- c("mean", "median", "pmf", "quantile") # nolint object_use_linter
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
get_metrics_character <- function(metrics, output_type) {
  if (output_type == "quantile") {
    # split into metrics for interval coverage and others
    interval_metric_inds <- grepl(pattern = "^interval_coverage_[[:digit:]][[:digit:]]$", metrics)
    interval_metrics <- metrics[interval_metric_inds]
    other_metrics <- metrics[!interval_metric_inds]

    # validate metrics
    valid_metrics <- c("ae_median", "wis", "overprediction", "underprediction", "dispersion")
    invalid_metrics <- other_metrics[!other_metrics %in% valid_metrics]
    valid_metrics <- c(valid_metrics, "interval_coverage_XY")
    if (length(invalid_metrics) > 0) {
      cli::cli_abort(
        c(
          "`metrics` had {length(invalid_metrics)} unsupported metric{?s} for",
          " `output_type` {.val {output_type}}: {.val {invalid_metrics}};",
          " supported metrics include {.val {valid_metrics}}",
          " where `XY` denotes the coverage level, e.g. \"interval_coverage_95\"."
        )
      )
    }

    # assemble metric functions
    interval_metric_fns <- lapply(
      interval_metrics,
      function(metric) {
        level <- as.integer(substr(metric, 19, 20))
        return(purrr::partial(scoringutils::interval_coverage, interval_range = level))
      }
    )
    names(interval_metric_fns) <- interval_metrics

    other_metric_fns <- scoringutils::metrics_quantile(select = other_metrics)

    metric_fns <- c(other_metric_fns, interval_metric_fns)[metrics]
    return(metric_fns)
  } else if (output_type == "pmf") {
    valid_metrics <- c("log_score")
    invalid_metrics <- metrics[!metrics %in% valid_metrics]
    if (length(invalid_metrics) > 0) {
      cli::cli_abort(
        c(
          "`metrics` had {length(invalid_metrics)} unsupported metric{?s} for",
          " `output_type` {.val{output_type}}: {.val {invalid_metrics}};",
          " supported metrics include {.val {valid_metrics}}."
        )
      )
    }
    metrics <- scoringutils::metrics_nominal(select = metrics)
  } else if (output_type %in% c("median", "mean")) {
    valid_metrics <- c("ae_point", "se_point")
    invalid_metrics <- metrics[!metrics %in% valid_metrics]
    if (length(invalid_metrics) > 0) {
      cli::cli_abort(
        c(
          "`metrics` had {length(invalid_metrics)} unsupported metric{?s} for",
          " `output_type` {.val{output_type}}: {.val {invalid_metrics}};",
          " supported metrics include {.val {valid_metrics}}."
        )
      )
    }
    metrics <- scoringutils::metrics_point(select = metrics)
  } else {
    # we have already validated `output_type`, so this case should not be
    # triggered; this case is just double checking in case we add something new
    # later, to ensure we update this function.
    supported_types <- c("mean", "median", "pmf", "quantile") # nolint object_use_linter
    cli::cli_abort(
      "Provided `model_out_tbl` contains `output_type` {.val {output_type}};
      hubEvals currently only supports the following types:
      {.val {supported_types}}"
    )
  }

  return(metrics)
}
