# Helper used in inline roxygen `r ` expressions to generate bulleted metric
# lists without exceeding the line length limit.

#' @noRd
.metrics_list <- function(
  forecast_example = NULL,
  class = NULL,
  exclude = NULL
) {
  if (!is.null(forecast_example)) {
    metrics <- scoringutils::get_metrics(forecast_example, exclude = exclude)
  } else {
    fn <- utils::getS3method(
      "get_metrics",
      class,
      envir = asNamespace("scoringutils")
    )
    metrics <- fn()
  }
  paste("- ", names(metrics), collapse = "\n")
}
