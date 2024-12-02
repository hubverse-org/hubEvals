library(hubExamples)


dput_fun <- function(obj, path = stdout()) {
  x <- get(obj)
  if (!is.function(x)) {
    x <- paste(
      capture.output(dput(x)),
      # capture.output(dput(x, control = c("keepNA", "keepInteger", "niceNames"))),
      collapse = "\n  "
    )
    res <- glue::glue("# nolint start\n{obj} <- function() {{\n  {dput(x)}\n}}\n# nolint end")
    writeLines(res, path)
  }
}

hubex_forecast_outputs <- hubExamples::forecast_outputs |>
  as.data.frame()
# attr(hubex_forecast_outputs, "problems") <- NULL
hubex_forecast_oracle_output <- hubExamples::forecast_oracle_output |>
  dplyr::filter(
    target_end_date >= "2022-10-01", target_end_date <= "2023-02-01",
    location %in% c("06", "25", "48")
  ) |>
  as.data.frame()
attr(hubex_forecast_oracle_output, "problems") <- NULL

dput_fun("hubex_forecast_outputs", "tests/testthat/helper-hubex_forecast_outputs.R")
dput_fun("hubex_forecast_oracle_output", "tests/testthat/helper-hubex_forecast_oracle_output.R")
