#' Transform quantile model output into a forecast object
#'
#' @param model_out_tbl Model output tibble with predictions
#' @param target_observations Observed 'ground truth' data to be compared against predictions
#'
#' @return forecast_quantile
transform_quantile_model_out <- function(model_out_tbl, target_observations) {
  model_out_tbl <- validate_model_out_target_obs(model_out_tbl, target_observations)

  task_id_cols <- get_task_id_cols(model_out_tbl)

  model_out_tbl <- model_out_tbl |>
    dplyr::filter(output_type == "quantile") |>
    dplyr::mutate(output_type_id = as.numeric(output_type_id)) |>
    dplyr::rename(model = model_id)

  if (c("output_type") %in% colnames(target_observations)) {
    target_observations <- target_observations |>
      dplyr::filter(output_type == "quantile") |>
      dplyr::select(-c("output_type", "output_type_id"))
  }

  data <- dplyr::left_join(model_out_tbl, target_observations,
    by = task_id_cols[task_id_cols %in% colnames(target_observations)],
    relationship = "many-to-one"
  )

  forecast_quantile <- scoringutils::as_forecast_quantile(data,
    forecast_unit = c("model", task_id_cols),
    observed = "observation",
    predicted = "value",
    model = "model",
    quantile_level = "output_type_id"
  )

  return(forecast_quantile)
}
