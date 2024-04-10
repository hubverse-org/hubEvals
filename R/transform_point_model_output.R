#' Transform mean/median model output into a forecast object
#'
#' @param proj_data Model output projection data frame; requires columns: model_id, target, value, location, target_end_date, horizon, output_type
#' @param obs_data Observed 'ground truth' data to be compared against forecasts; requires columns: target, observation, location, target_end_date, output_type
#' @param type Forecast output type: "mean" or "median"
#'
#' @return forecast_point
#'
#' @importFrom dplyr %>%
#'
#' @examples

transform_point_model_output <- function(proj_data, obs_data, type) {

  proj_data <- proj_data %>%
    dplyr::filter(output_type == type) %>%
    dplyr::mutate(model = model_id)

  obs_data <- obs_data %>%
    dplyr::filter(output_type == type)

  data <- dplyr::left_join(proj_data, obs_data,
                           by = c("target","location","target_end_date","output_type"))

  forecast_point <- scoringutils::as_forecast(data,
                                                 forecast_unit = c("model","location","reference_date","target_end_date","horizon"),
                                                 forecast_type = "point",
                                                 observed = "observation",
                                                 predicted = "value",
                                                 model = "model")

  return(forecast_point)
}
