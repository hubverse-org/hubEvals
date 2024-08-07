#' Transform quantile model output into a forecast object
#'
#' @param model_out_tbl Forecast model output tibble
#' @param target_data Observed 'ground truth' data to be compared against forecasts
#'
#' @return forecast_quantile
transform_quantile_model_out <- function(model_out_tbl, target_data) {
  # check that: model_out_tbl contains columns: model_id, output_type, output_type_id, value
  req_cols <- c("model_id", "output_type", "output_type_id", "value")
  if (!all(req_cols %in% colnames(model_out_tbl))) {
    cli::cli_abort(
      "model_out_tbl does not contain required columns: model_id, output_type, output_type_id, value:
      {.val {colnames(model_out_tbl)}}"
    )
  }

  model_out_cols <- colnames(model_out_tbl)
  non_task_cols <- c(
    "model_id", "output_type", "output_type_id",
    "value", "model_abbr", "team_abbr"
  )
  task_id_cols <- model_out_cols[!model_out_cols %in% non_task_cols]

  # check that model_out_tbl and target_data have compatible columns
  if (length(task_id_cols[task_id_cols %in% colnames(target_data)]) == 0) {
    cli::cli_abort(
      "model_out_tbl and target_data do not have compatible columns"
    )
  }

  if (!c("observation") %in% colnames(target_data)) {
    cli::cli_abort(
      "target_data does not have observation column"
    )
  }

  if (!inherits(model_out_tbl, "model_out_tbl")) {
    model_out_tbl <- hubUtils::as_model_out_tbl(model_out_tbl)
  }

  model_out_tbl <- model_out_tbl |>
    dplyr::filter(output_type == "quantile") |>
    dplyr::mutate(output_type_id = as.numeric(output_type_id)) |>
    dplyr::rename(model = model_id)

  if (c("output_type") %in% colnames(target_data)) {
    target_data <- target_data |>
      dplyr::filter(output_type == "quantile") |>
      dplyr::select(-c("output_type", "output_type_id"))
  }

  data <- dplyr::left_join(model_out_tbl, target_data,
    by = task_id_cols[task_id_cols %in% colnames(target_data)],
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
