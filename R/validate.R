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

  return(task_id_cols)
}

#' Validate model_out_tbl and target_observations arguments to transform functions
#'
#' @param model_out_tbl Model output tibble with predictions
#' @param target_observations Observed 'ground truth' data to be compared against predictions
#'
#' @return The input `model_out_tbl`, possibly modified to ensure it has S3 class `model_out_tbl`
#'
#' @noRd
validate_model_out_target_obs <- function(model_out_tbl, target_observations) {
  # check that: model_out_tbl contains columns: model_id, output_type, output_type_id, value
  req_cols <- c("model_id", "output_type", "output_type_id", "value")
  if (!all(req_cols %in% colnames(model_out_tbl))) {
    cli::cli_abort(
      "model_out_tbl does not contain required columns: model_id, output_type, output_type_id, value:
      {.val {colnames(model_out_tbl)}}"
    )
  }

  # check that model_out_tbl and target_observations have compatible columns
  task_id_cols <- get_task_id_cols(model_out_tbl)
  if (length(task_id_cols[task_id_cols %in% colnames(target_observations)]) == 0) {
    cli::cli_abort(
      "model_out_tbl and target_observations do not have compatible columns"
    )
  }
  t_o_cols <- colnames(target_observations)
  expected_cols_superset <- c(task_id_cols, "output_type", "output_type_id", "observation")
  unexpected_cols <- t_o_cols[!t_o_cols %in% expected_cols_superset]
  if (length(unexpected_cols) > 0) {
    cli::cli_abort(
      c(
        "`target_observations` had {length(unexpected_cols)} unexpected column{?s} {.val {unexpected_cols}};",
        " expected the columns of `target_observations` to be a subset of {.val {expected_cols_superset}}."
      )
    )
  }

  if (!c("observation") %in% colnames(target_observations)) {
    cli::cli_abort(
      "target_observations does not have observation column"
    )
  }

  if (!inherits(model_out_tbl, "model_out_tbl")) {
    model_out_tbl <- hubUtils::as_model_out_tbl(model_out_tbl)
  }

  return(model_out_tbl)
}
