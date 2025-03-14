#' Get the names of columns in model_out_tbl corresponding to task IDs
#'
#' @param model_out_tbl Model output tibble with predictions
#'
#' @return character vector of column names
#'
#' @noRd
get_task_id_cols <- function(model_out_tbl) {
  model_out_cols <- colnames(model_out_tbl)
  # To-do: hubUtils::std_colnames (non-task id cols)
  non_task_cols <- c(
    "model_id", "output_type", "output_type_id",
    "value", "model_abbr", "team_abbr"
  )
  task_id_cols <- model_out_cols[!model_out_cols %in% non_task_cols]

  task_id_cols
}

#' Validate model_out_tbl and oracle_output arguments to transform functions
#'
#' @param model_out_tbl Model output tibble with predictions
#' @param oracle_output Predictions that would have been generated by an oracle
#' model that knew the observed target data values in advance
#'
#' @return The input `model_out_tbl`, possibly modified to ensure it has S3 class `model_out_tbl`
#'
#' @noRd
validate_model_oracle_out <- function(model_out_tbl, oracle_output) {
  # check that: model_out_tbl contains columns: model_id, output_type, output_type_id, value
  req_cols <- c("model_id", "output_type", "output_type_id", "value")
  if (!all(req_cols %in% colnames(model_out_tbl))) {
    cli::cli_abort(
      "model_out_tbl does not contain required columns: model_id, output_type, output_type_id, value:
      {.val {colnames(model_out_tbl)}}"
    )
  }

  # check that model_out_tbl and oracle_output have compatible columns
  task_id_cols <- get_task_id_cols(model_out_tbl)
  if (length(task_id_cols[task_id_cols %in% colnames(oracle_output)]) == 0) {
    cli::cli_abort(
      "model_out_tbl and oracle_output do not have compatible columns"
    )
  }
  t_o_cols <- colnames(oracle_output)
  expected_cols_superset <- c(task_id_cols, "output_type", "output_type_id", "oracle_value")
  unexpected_cols <- t_o_cols[!t_o_cols %in% expected_cols_superset]
  if (length(unexpected_cols) > 0) {
    cli::cli_abort(
      c(
        "`oracle_output` had {length(unexpected_cols)} unexpected column{?s} {.val {unexpected_cols}};",
        " expected the columns of `oracle_output` to be a subset of {.val {expected_cols_superset}}."
      )
    )
  }

  if (!c("oracle_value") %in% colnames(oracle_output)) {
    cli::cli_abort(
      "oracle_output does not have oracle_value column"
    )
  }

  if (!inherits(model_out_tbl, "model_out_tbl")) {
    model_out_tbl <- hubUtils::as_model_out_tbl(model_out_tbl)
  }

  model_out_tbl
}


#' Check that model_out_tble has a single `output_type` that is one of the
#' `output_types` that is supported by this function.
#'
#' @return if valid, the output_type in model_out_tbl
#'
#' @noRd
validate_output_type <- function(model_out_tbl) {
  output_type <- unique(model_out_tbl$output_type)
  if (length(output_type) != 1) {
    cli::cli_abort(
      "model_out_tbl must contain a single output_type, but it has multiple:
      {.val {output_type}}"
    )
  }

  error_if_invalid_output_type(output_type)

  output_type
}


error_if_invalid_output_type <- function(output_type) {
  supported_types <- c("mean", "median", "pmf", "quantile")
  if (!output_type %in% supported_types) {
    cli::cli_abort(
      "Provided `model_out_tbl` contains `output_type` {.val {output_type}};
      hubEvals currently only supports the following types:
      {.val {supported_types}}"
    )
  }
}


#' Validate relative metrics
#'
#' @noRd
validate_relative_metrics <- function(relative_metrics, metrics, by) {
  if (any(is_interval_coverage_metric(relative_metrics))) {
    cli::cli_abort(
      "Interval coverage metrics are not supported for relative skill scores."
    )
  }

  if (length(relative_metrics) > 0 && !"model_id" %in% by) {
    cli::cli_abort(
      "Relative metrics require 'model_id' to be included in {.arg by}."
    )
  }

  extra_metrics <- setdiff(relative_metrics, metrics)
  if (length(extra_metrics) > 0) {
    cli::cli_abort(c(
      "Relative metrics must be a subset of the metrics.",
      "x" = "The following {.arg relative_metrics} are not in {.arg metrics}: {.val {extra_metrics}}"
    ))
  }
}
