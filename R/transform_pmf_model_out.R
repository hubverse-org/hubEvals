#' Transform pmf model output into a forecast object
#'
#' @param model_out_tbl Model output tibble with predictions
#' @param target_observations Observed 'ground truth' data to be compared against predictions
#' @param output_type_id_order For nominal variables, this should be `NULL` (the default).
#' For ordinal variables, this is a vector of levels for pmf forecasts, in
#' increasing order of the levels.
#'
#' @return forecast_quantile
transform_pmf_model_out <- function(model_out_tbl, target_observations, output_type_id_order = NULL) {
  model_out_tbl <- validate_model_out_target_obs(model_out_tbl, target_observations)

  # subset both model_out_tbl and target_observations to output_type == "pmf"
  model_out_tbl <- model_out_tbl |>
    dplyr::filter(output_type == "pmf")

  if (c("output_type") %in% colnames(target_observations)) {
    target_observations <- target_observations |>
      dplyr::filter(output_type == "pmf") |>
      dplyr::select(-output_type)
  }

  # validate or set output_type_id_order
  if (!is.null(output_type_id_order)) {
    cli::cli_abort(
      "ordinal variables are not yet supported (we expect that they will be by the time we release this package)."
    )
    is_ordinal <- TRUE
  } else {
    is_ordinal <- FALSE
    output_type_id_order <- unique(model_out_tbl$output_type_id)
  }

  task_id_cols <- get_task_id_cols(model_out_tbl)

  # assemble data in scoringutils format
  model_out_tbl <- model_out_tbl |>
    dplyr::rename(model = model_id)

  data <- dplyr::left_join(
    model_out_tbl, target_observations,
    by = c(task_id_cols[task_id_cols %in% colnames(target_observations)], "output_type_id"),
    relationship = "many-to-one"
  ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(task_id_cols, "model")))) |>
    dplyr::mutate(
      observation = output_type_id[observation == 1],
      observation = factor(observation, levels = output_type_id_order, ordered = is_ordinal),
      output_type_id = factor(output_type_id, levels = output_type_id_order, ordered = is_ordinal)
    )

  forecast_pmf <- scoringutils::as_forecast_nominal(
    data,
    forecast_unit = c("model", task_id_cols),
    observed = "observation",
    predicted = "value",
    model = "model",
    predicted_label = "output_type_id"
  )

  return(forecast_pmf)
}
