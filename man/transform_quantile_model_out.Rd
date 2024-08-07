% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_quantile_model_out.R
\name{transform_quantile_model_out}
\alias{transform_quantile_model_out}
\title{Transform quantile model output into a forecast object}
\usage{
transform_quantile_model_out(model_out_tbl, target_data)
}
\arguments{
\item{model_out_tbl}{Forecast model output tibble}

\item{target_data}{Observed 'ground truth' data to be compared against forecasts}
}
\value{
forecast_quantile
}
\description{
Transform quantile model output into a forecast object
}
