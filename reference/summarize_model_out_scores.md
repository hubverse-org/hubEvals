# Summarize per-forecast scores

Summarizes scores computed by
[`score_model_out()`](https://hubverse-org.github.io/hubEvals/reference/score_model_out.md)
with `summarize = FALSE`. The result is the same as calling
[`score_model_out()`](https://hubverse-org.github.io/hubEvals/reference/score_model_out.md)
with `summarize = TRUE` on the forecasts those scores came from, but the
forecasts only need to be scored once. This is useful when the same
scores are summarized several ways, for example by different `by`
columns or over different subsets of the forecasts.

## Usage

``` r
summarize_model_out_scores(
  scores,
  relative_metrics = NULL,
  baseline = NULL,
  by = "model_id",
  include_count = FALSE
)
```

## Arguments

- scores:

  Per-forecast scores, as returned by
  `score_model_out(summarize = FALSE)`, or a subset of their rows.

- relative_metrics:

  Character vector of scoring metrics for which to compute relative
  skill scores. Each must be one of the metrics in `scores`, and
  relative skill requires that `"model_id"` is included in `by`. See
  [`score_model_out()`](https://hubverse-org.github.io/hubEvals/reference/score_model_out.md)
  for which metrics are supported and how comparison groups are formed.
  If `NULL` (the default), no relative metrics are computed.

- baseline:

  String with the name of a model to use as a baseline for relative
  skill scores. If a baseline is given, then a scaled relative skill
  with respect to the baseline will be returned. By default (`NULL`),
  relative skill will not be scaled with respect to a baseline model. If
  the baseline is absent from a comparison group, that group's relative
  and scaled relative skill are reported as `NA` (with a warning) while
  its absolute scores are still returned. This `NA` case only arises
  when scoring is disaggregated: with `by = "model_id"` alone there is a
  single comparison group containing every model, so relative skill is
  always defined and is never `NA`. A baseline absent from the data
  entirely is an error.

- by:

  Character vector naming columns to summarize by. For example,
  specifying `by = "model_id"` (the default) will compute average scores
  for each model.

- include_count:

  Boolean indicator of whether to add a column `count` giving the number
  of forecasts scored in each summary group. Defaults to `FALSE`.

## Value

A tibble of summarized scores, in the same form as
[`score_model_out()`](https://hubverse-org.github.io/hubEvals/reference/score_model_out.md)
returns with `summarize = TRUE`.

## Details

If the scores were computed with `transform_append = TRUE` and a
`transform`, average scores are reported separately for the natural and
transformed scales (the `scale` column), as in
[`score_model_out()`](https://hubverse-org.github.io/hubEvals/reference/score_model_out.md).

Relative skill is computed within each group of the `by` columns other
than `"model_id"`, using the forecasts in `scores`. Summarizing a subset
of the scored forecasts therefore gives the same relative skill as
scoring only that subset.

Scores that have already been summarized can't be summarized again,
since averaging group means gives the wrong result when groups have
different numbers of forecasts.

## Examples

``` r
model_out_tbl <- hubExamples::forecast_outputs |>
  dplyr::filter(.data[["output_type"]] == "quantile")
oracle_output <- hubExamples::forecast_oracle_output

# score once
row_scores <- score_model_out(
  model_out_tbl,
  oracle_output,
  metrics = c("wis", "interval_coverage_90"),
  summarize = FALSE
)

# summarize two ways without scoring again
summarize_model_out_scores(row_scores, relative_metrics = "wis")
#> # A tibble: 3 × 4
#>   model_id            wis interval_coverage_90 wis_relative_skill
#>   <chr>             <dbl>                <dbl>              <dbl>
#> 1 Flusight-baseline  329.                0.125              1.15 
#> 2 MOBS-GLEAM_FLUH    315.                0.562              1.10 
#> 3 PSI-DICE           228.                0.5                0.794
summarize_model_out_scores(
  row_scores,
  relative_metrics = "wis",
  by = c("model_id", "location")
)
#> # A tibble: 6 × 5
#>   model_id          location   wis interval_coverage_90 wis_relative_skill
#>   <chr>             <chr>    <dbl>                <dbl>              <dbl>
#> 1 Flusight-baseline 25        180.                0.25               1.13 
#> 2 Flusight-baseline 48        479.                0                  1.16 
#> 3 MOBS-GLEAM_FLUH   25        163.                0.5                1.02 
#> 4 MOBS-GLEAM_FLUH   48        468.                0.625              1.13 
#> 5 PSI-DICE          25        139.                0.625              0.872
#> 6 PSI-DICE          48        317.                0.375              0.765
```
