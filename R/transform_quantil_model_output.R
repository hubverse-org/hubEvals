#' Transform quantile model output into a forecast object
#'
#' @param proj_data Model output projection data frame; requires columns: model_id, target, value, location, target_end_date, horizon, output_type, output_type_id
#' @param obs_data Observed 'ground truth' data to be compared against forecasts; requires columns: target, observation, location, target_end_date, output_type
#'
#' @return forecast_quantile
#'
#' @importFrom dplyr %>%
#'
#' @examples

transform_quantil_model_output <- function(proj_data, obs_data) {

  proj_data <- proj_data %>%
    dplyr::filter(output_type == "quantile") %>%
    dplyr::mutate(output_type_id = as.numeric(output_type_id),
                  model = model_id)

  obs_data <- obs_data %>%
    dplyr::filter(output_type == "quantile") %>%
    dplyr::select(-output_type_id)

  data <- dplyr::left_join(proj_data, obs_data,
                           by = c("target", "location", "target_end_date", "output_type"))

  forecast_quantile <- scoringutils::as_forecast(data,
                                                 forecast_unit = c("model", "location", "reference_date",
                                                                   "target_end_date", "horizon"),
                                                 forecast_type = "quantile",
                                                 observed = "observation",
                                                 predicted = "value",
                                                 model = "model_id",
                                                 quantile_level = "output_type_id")

  return(forecast_quantile)
}
