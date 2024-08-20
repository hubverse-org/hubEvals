#' Score model output predictions against observed data
#'
#' @param model_out_tbl Model output tibble with predictions
#' @param target_observations Observed 'ground truth' data to be compared against predictions
#' @param metrics Optional list of scoring metrics to computeq
#' @param output_type_id_order For ordinal variables in pmf format, this is a vector of levels for pmf forecasts, in
#' increasing order of the levels. For all other output types, this is ignored.
#'
#' @return forecast_quantile
#'
#' @export
score_model_out <- function(model_out_tbl, target_observations, metrics = NULL, by = NULL,
                            output_type_id_order = NULL) {
  # check that model_out_tbl has a single output_type that is supported by this package
  output_type <- validate_output_type(model_out_tbl)

  # assemble data for scoringutils
  if (output_type == "quantile") {
    su_data <- transform_quantile_model_out(model_out_tbl, target_observations)
    if (is.null(metrics)) {
      metrics <- scoringutils::metrics_quantile()
    }
  } else if (output_type == "pmf") {
    su_data <- transform_pmf_model_out(model_out_tbl, target_observations, output_type_id_order)
    if (is.null(metrics)) {
      metrics <- scoringutils::metrics_nominal()
    }
  } else if (output_type %in% c("mean", "median")) {
    su_data <- transform_point_model_out(model_out_tbl, target_observations, output_type)
  }

  # compute scores
  scores <- scoringutils::score(su_data, metrics)

  return(scores)
}
