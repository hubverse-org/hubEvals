#' Transform mean/median model output into a forecast object
#'
#' @param model_out_tbl Model output projection data frame;
#' requires columns: model_id, target, value, location, target_end_date, horizon, output_type
#' @param target_data Observed 'ground truth' data to be compared against forecasts;
#' requires columns: target, observation, location, target_end_date, output_type
#' @param output_type Forecast output type: "mean" or "median"
#'
#' @return forecast_point
#'
#' @importFrom dplyr %>%
#'
#' @examples

transform_point_model_out <- function(model_out_tbl, target_data, output_type) {

  if(!inherits(model_out_tbl, "model_out_tbl")) {
    model_outputs <- hubUtils::as_model_out_tbl(model_out_tbl)
  }

  if((!inherits(output_type, "character")) || (!output_type %in% c("mean","median"))) {
    cli::cli_abort("invalid 'output_type': {.val {output_type}} Must be 'mean' or 'median'")
  }

  model_out_tbl <- model_out_tbl %>%
    dplyr::filter(output_type == output_type) %>%
    dplyr::mutate(model = model_id)

  target_data <- target_data %>%
    dplyr::filter(output_type == output_type)


# Join task IDs with output_type
  target_data <- dplyr::mutate(target_data, target_end_date = date)

  data <- dplyr::left_join(model_out_tbl, target_data,
                           by = c("location", "target_end_date"))

  forecast_point <- scoringutils::as_forecast(data,
                                              forecast_unit = c("model", "location", "reference_date",
                                                                "target_end_date", "horizon"),
                                              forecast_type = "point",
                                              observed = "observation",
                                              predicted = "value",
                                              model = "model")

  return(forecast_point)
}
