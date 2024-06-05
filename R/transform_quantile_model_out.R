#' Transform quantile model output into a forecast object
#'
#' @param model_out_tbl Forecast model output tibble
#' @param target_data Observed 'ground truth' data to be compared against forecasts
#'
#' @return forecast_quantile
#'
#' @importFrom dplyr %>%
#'
#' @examples
transform_quantile_model_out <- function(model_out_tbl, target_data) {
  model_out_cols <- colnames(model_out_tbl)
  non_task_cols <- c(
    "model_id", "output_type", "output_type_id",
    "value", "model_abbr", "team_abbr"
  )
  task_id_cols <- model_out_cols[!model_out_cols %in% non_task_cols]

  model_out_tbl <- model_out_tbl %>%
    dplyr::filter(output_type == "quantile") %>%
    dplyr::mutate(
      output_type_id = as.numeric(output_type_id)
    )

  if (c("output_type") %in% colnames(target_data)) {
    target_data <- target_data %>%
      dplyr::filter(output_type == "quantile") %>%
      dplyr::select(-c("output_type", "output_type_id"))
  }

  data <- dplyr::left_join(model_out_tbl, target_data,
    by = task_id_cols[task_id_cols %in% colnames(target_data)],
    relationship = "many-to-one"
  )

  forecast_quantile <- scoringutils::as_forecast(data,
    forecast_unit = c("model_id", task_id_cols),
    forecast_type = "quantile",
    observed = "observation",
    predicted = "value",
    model = "model_id",
    quantile_level = "output_type_id"
  )

  return(forecast_quantile)
}
