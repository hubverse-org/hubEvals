#' Transform either mean or median model output into a point forecast object:
#'
#'
#' @param model_out_tbl Model output tibble with predictions
#' @param target_observations Observed 'ground truth' data to be compared against predictions
#' @param output_type Forecast output type: "mean" or "median"
#'
#' @return forecast_point
#'
#' @details This function transforms a model output tibble in the Hubverse
#' format (with either "mean" or "median" output type) to a scoringutils "point"
#' forecast object
transform_point_model_out <- function(model_out_tbl, target_observations, output_type) {
  if ((!inherits(output_type, "character")) || (!output_type %in% c("mean", "median"))) {
    cli::cli_abort(
      "invalid 'output_type': {.val {output_type}} Must be 'mean' or 'median'"
    )
  }

  model_out_tbl <- validate_model_out_target_obs(model_out_tbl, target_observations)

  task_id_cols <- get_task_id_cols(model_out_tbl)
  type <- output_type

  model_out_tbl <- model_out_tbl |>
    dplyr::filter(output_type == type) |>
    dplyr::rename(model = model_id)

  if (c("output_type") %in% colnames(target_observations)) {
    target_observations <- target_observations |>
      dplyr::filter(output_type == type) |>
      dplyr::select(-c("output_type", "output_type_id"))
  }

  data <- dplyr::left_join(model_out_tbl, target_observations,
    by = task_id_cols[task_id_cols %in% colnames(target_observations)],
    relationship = "many-to-one"
  )

  forecast_point <- scoringutils::as_forecast_point(data,
    forecast_unit = c("model", task_id_cols),
    observed = "observation",
    predicted = "value",
    model = "model"
  )

  return(forecast_point)
}
