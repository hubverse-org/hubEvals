% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_point_model_out.R
\name{transform_point_model_out}
\alias{transform_point_model_out}
\title{Transform either mean or median model output into a point forecast object:}
\usage{
transform_point_model_out(model_out_tbl, target_data, output_type)
}
\arguments{
\item{model_out_tbl}{Forecast model output tibble}

\item{target_data}{Observed 'ground truth' data to be compared against forecasts}

\item{output_type}{Forecast output type: "mean" or "median"}
}
\value{
forecast_point
}
\description{
Transform either mean or median model output into a point forecast object:
}
\details{
This function transforms a forecast output tibble from the Hubverse
format (with either "mean" or "median" output type) to a scoringutils "point"
forecast object
}
