#' Example model output data representing quantile forecasts
#'
#' These are a subset of forecasts of daily incident hospital admissions for
#' COVID-19 that were submitted to the US COVID-19 Forecast hub, which have
#' been lightly modified for expository purposes.
#'
#' @format ## `example_quantile_model_output`
#' A data frame with 112 rows and 8 columns:
#' \describe{
#'   \item{origin_date}{Date: Reference date for the forecast.}
#'   \item{horizon}{integer: Forecast horizon, in days.}
#'   \item{location}{character: FIPS codes identifying the location being forecasted.}
#'   \item{target}{character: The forecast target. All values are "inc covid hosp".}
#'   \item{output_type}{character: Output type used to represent the forecast. All values are "quantile".}
#'   \item{output_type_id}{numeric: The probability level for the quantile forecast. One of 0.05, 0.1, 0.25, 0.5, 0.75, 0.9 or 0.95.}
#'   \item{value}{integer: The quantile of the forecast distribution.}
#'   \item{model_id}{character: Identifier for the model that produced the forecast.}
#' }
#' @source <https://github.com/reichlab/covid19-forecast-hub>
"example_quantile_model_output"


#' Example target data recording outcomes that are predicted in the
#' forecasts stored in `example_quantile_model_output`.
#'
#' These are daily incident hospital admissions for COVID-19 that were reported
#' on HealthData.gov. They are organized so as to match the forecast task id
#' columns in `example_quantile_model_output`, so that the observed data can be
#' joined into the forecast data frame correctly.
#'
#' @format ## `example_target_data`
#' A data frame with 8 rows and 5 columns:
#' \describe{
#'   \item{origin_date}{Date: Reference date for the forecast.}
#'   \item{horizon}{integer: Forecast horizon, in days.}
#'   \item{location}{character: FIPS codes identifying the location being forecasted.}
#'   \item{target}{character: The forecast target. All values are "inc covid hosp".}
#'   \item{value}{integer: The number of reported hospitalizations.}
#' }
#' @source <https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh/about_data>
"example_target_data"

