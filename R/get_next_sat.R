#' Get next Saturday
#'
#' @param date Date forecast begins (forecast_date)
#'
#' @return Date of next Saturday
#' @export
#'
#' @examples
#'

get_next_sat <- function(date) {
  require(lubridate)
  date <- as.Date(date)
  diff <- 7 - wday(date)
  new_date <- diff + date
  return(new_date)
}
