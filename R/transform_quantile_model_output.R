#' Title
#'
#' @param proj_data
#' @param obs_data
#'
#' @return
#' @export
#'
#' @examples

transform_quantile_model_output <- function(proj_data, obs_data) {

  data <- left_join(proj_data, obs_data, by = c("target","location","target_end_date","output_type"))

  forecast <- data %>%
    filter(data, output_type == "quantile") %>%
    scoringutils::as_forecast(
      forecast_unit = c("model","location","target_end_date","horizon"),
      forecast_type = "quantile",
      observed = "observed",
      predicted = "predicted",
      model = "model",
      quantile_level = "quantile_level"
    )


  return(forecast_quantile)

}
