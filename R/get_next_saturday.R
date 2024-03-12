#' Get next Saturday
#'
#' @param date
#'
#' @return Date of next Saturday
#' @export
#'
#' @examples
#'

get_next_saturday <- function(date) {
    require(lubridate)
    date <- as.Date(date)
    diff <- 7 - wday(date)
    new_date <- diff + date
    return(new_date)
}
