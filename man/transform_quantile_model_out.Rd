% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_quantile_model_out.R
\name{transform_quantile_model_out}
\alias{transform_quantile_model_out}
\title{Transform quantile model output into a forecast object}
\usage{
transform_quantile_model_out(model_out_tbl, target_observations)
}
\arguments{
\item{model_out_tbl}{Model output tibble with predictions}

\item{target_observations}{Observed 'ground truth' data to be compared against predictions}
}
\value{
forecast_quantile
}
\description{
Transform quantile model output into a forecast object
}
