#' Transform either mean or median model output into a point forecast object:
#'
#'
#' @param model_out_tbl Forecast model output tibble
#' @param target_data Observed 'ground truth' data to be compared against forecasts
#' @param output_type Forecast output type: "mean" or "median"
#'
#' @return forecast_point
#'
#' @details This function transforms a forecast output tibble from the Hubverse
#' format (with either "mean" or "median" output type) to a scoringutils "point"
#' forecast object
transform_point_model_out <- function(model_out_tbl, target_data, output_type) {
  if ((!inherits(output_type, "character")) || (!output_type %in% c("mean", "median"))) {
    cli::cli_abort(
      "invalid 'output_type': {.val {output_type}} Must be 'mean' or 'median'"
    )
  }

  # check that: model_out_tbl contains columns: model_id, output_type, output_type_id, value
  req_cols <- c("model_id", "output_type", "output_type_id", "value")
  if (!all(req_cols %in% colnames(model_out_tbl))) {
    cli::cli_abort(
      "model_out_tbl does not contain required columns: model_id, output_type, output_type_id, value:
      {.val {colnames(model_out_tbl)}}"
    )
  }

  model_out_cols <- colnames(model_out_tbl)
  # To-do: hubUtils::std_colnames (non-task id cols)
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

  type <- output_type

  model_out_tbl <- model_out_tbl |>
    dplyr::filter(output_type == type) |>
    dplyr::rename(model = model_id)

  if (c("output_type") %in% colnames(target_data)) {
    target_data <- target_data |>
      dplyr::filter(output_type == type) |>
      dplyr::select(-c("output_type", "output_type_id"))
  }

  data <- dplyr::left_join(model_out_tbl, target_data,
    by = task_id_cols[task_id_cols %in% colnames(target_data)],
    relationship = "many-to-one"
  )

  forecast_point <- scoringutils::as_forecast(data,
    forecast_unit = c("model", task_id_cols),
    forecast_type = "point",
    observed = "observation",
    predicted = "value",
    model = "model"
  )

  return(forecast_point)
}
