# hubEvals

The goal of hubEvals is to provide tools for evaluating infectious
disease model outputs. This package is part of the
[Hubverse](https://hubverse.org) project, which aims to provide a suite
of tools for infectious disease modeling hubs.

## Installation

### Latest

You can install the [latest version of hubEvals from the
R-universe](https://hubverse-org.r-universe.dev/hubEvals):

``` r
install.packages("hubEvals", repos = c("https://hubverse-org.r-universe.dev", "https://cloud.r-project.org"))
```

### Development

If you want to test out new features that have not yet been released,
you can install the development version of hubEvals from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("hubverse-org/hubEvals")
```

## Example

Predictions can be evaluated directly using the scoring function in
`hubEvals`, which assumes a hubverse format for the model outputs and
target data:

``` r
library(hubEvals)

# compute default metrics (in this case, absolute error) for
# median forecasts, summarized by the mean score for each model
median_scores <- score_model_out(
  model_out_tbl = hubExamples::forecast_outputs |>
    dplyr::filter(output_type == "median"), # only one output type allowed
  oracle_output = hubExamples::forecast_oracle_output,
  by = "model_id"
)
median_scores
#>             model_id ae_point
#>               <char>    <num>
#> 1: Flusight-baseline  401.875
#> 2:   MOBS-GLEAM_FLUH  416.375
#> 3:          PSI-DICE  277.000
```

``` r

# compute WIS and interval coverage rates at 80% and 90% levels based on
# quantile forecasts, summarized by the mean score for each model
quantile_scores <- score_model_out(
  model_out_tbl = hubExamples::forecast_outputs |>
    dplyr::filter(output_type == "quantile"), # only one output type allowed
  oracle_output = hubExamples::forecast_oracle_output,
  metrics = c("wis", "interval_coverage_80", "interval_coverage_90"),
  relative_metrics = "wis",
  by = "model_id"
)
quantile_scores
#> Key: <model_id>
#>             model_id      wis interval_coverage_80 interval_coverage_90 wis_relative_skill
#>               <char>    <num>                <num>                <num>              <num>
#> 1: Flusight-baseline 329.4545                  0.0               0.1250          1.1473659
#> 2:   MOBS-GLEAM_FLUH 315.2393                  0.5               0.5625          1.0978597
#> 3:          PSI-DICE 227.9527                  0.5               0.5000          0.7938733
```

``` r

# compute log scores based on pmf predictions for categorical targets,
# summarized by the mean score for each combination of model and location.
# Note: if the model_out_tbl had forecasts for multiple targets using a
# pmf output_type with different bins, it would be necessary to score the
# predictions for those targets separately.
pmf_scores <- score_model_out(
  model_out_tbl = hubExamples::forecast_outputs |>
    dplyr::filter(output_type == "pmf"), # only one output type allowed
  oracle_output = hubExamples::forecast_oracle_output,
  metrics = c("log_score", "rps"),
  by = c("model_id", "location", "horizon"),
  output_type_id_order = c("low", "moderate", "high", "very high")
)
head(pmf_scores)
#>             model_id location horizon   log_score          rps
#>               <char>   <char>   <int>       <num>        <num>
#> 1: Flusight-baseline       25       0  0.02107606 0.0008531043
#> 2: Flusight-baseline       25       1  6.69652380 0.5029240066
#> 3: Flusight-baseline       25       2 17.73313203 1.0057355863
#> 4: Flusight-baseline       25       3         Inf 1.8665126816
#> 5: Flusight-baseline       48       0  2.18418007 0.4873966597
#> 6: Flusight-baseline       48       1  7.49960792 0.9659026096
```

Or, users may transform predictions into a `forecast` object that can be
used as an input to `scoringutils` functions and use their tooling
directly.

``` r
median_forecast <- transform_point_model_out(
  model_out_tbl = hubExamples::forecast_outputs |>
    dplyr::filter(output_type == "median"),
  oracle_output = hubExamples::forecast_oracle_output,
  output_type = "median"
)
median_forecast

quantile_forecast <- transform_quantile_model_out(
  model_out_tbl = hubExamples::forecast_outputs |>
    dplyr::filter(output_type == "quantile"),
  oracle_output = hubExamples::forecast_oracle_output
)
quantile_forecast

pmf_forecasts <- transform_pmf_model_out(
  model_out_tbl = hubExamples::forecast_outputs |>
    dplyr::filter(output_type == "pmf"),
  oracle_output = hubExamples::forecast_oracle_output,
  output_type_id_order = c("low", "moderate", "high", "very high")
)
pmf_forecasts
```

## Code of Conduct

Please note that the hubEvals package is released with a [Contributor
Code of
Conduct](https://hubverse-org.github.io/hubEvals/CODE_OF_CONDUCT.md). By
contributing to this project, you agree to abide by its terms.

## Contributing

Interested in contributing back to the open-source Hubverse project?
Learn more about how to [get involved in the Hubverse
Community](https://hubverse.io/community/) or [how to contribute to the
hubEvals
package](https://hubverse-org.github.io/hubEvals/CONTRIBUTING.md).
