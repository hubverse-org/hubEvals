# Getting started with hubEvals

`hubEvals` provides tools for scoring and evaluating
[hubverse](https://hubverse.io)-formatted model outputs, typically from
a forecasting hub. It bridges the hubverse
[`model_out_tbl`](https://docs.hubverse.io/en/latest/user-guide/model-output.html)
format and the [`scoringutils`](https://epiforecasts.io/scoringutils/)
package, so that an analyst can move from a tibble of model outputs plus
the matching [oracle
output](https://docs.hubverse.io/en/latest/user-guide/target-data.html#oracle-output)
target data to a tibble of scores in a single call.

This vignette walks through the main scoring workflows for each output
type supported by hubEvals, using example data from the
[`hubExamples`](https://hubverse-org.github.io/hubExamples/) package.
The example data come from the
[example-complex-forecast-hub](https://github.com/hubverse-org/example-complex-forecast-hub),
a small synthetic flu forecasting hub.

``` r

library(hubEvals)
library(dplyr)
```

## The input data

`hubEvals` consumes two tibbles:

- a
  **[`model_out_tbl`](https://docs.hubverse.io/en/latest/user-guide/model-output.html)**
  of model predictions (one row per prediction)
- an
  **[`oracle_output`](https://docs.hubverse.io/en/latest/user-guide/target-data.html#oracle-output)**
  of target data at the same task-id resolution (one row per
  observation; the `oracle_value` column holds the observed value)

[`hubExamples::forecast_outputs`](https://rdrr.io/pkg/hubExamples/man/forecast_data.html)
is a `model_out_tbl` that contains predictions in the five hubverse
output types currently supported by hubEvals: `quantile`, `mean`,
`median`, `pmf`, and `sample`. (The hubverse spec also defines a `cdf`
output type, evaluation of which is not yet supported.)

``` r

head(hubExamples::forecast_outputs)
#> # A tibble: 6 × 9
#>   model_id          reference_date target          horizon location target_end_date output_type output_type_id value
#>   <chr>             <date>         <chr>             <int> <chr>    <date>          <chr>       <chr>          <dbl>
#> 1 Flusight-baseline 2022-11-19     wk inc flu hosp       0 25       2022-11-19      quantile    0.05              22
#> 2 Flusight-baseline 2022-11-19     wk inc flu hosp       0 25       2022-11-19      quantile    0.1               31
#> 3 Flusight-baseline 2022-11-19     wk inc flu hosp       0 25       2022-11-19      quantile    0.25              45
#> 4 Flusight-baseline 2022-11-19     wk inc flu hosp       0 25       2022-11-19      quantile    0.5               51
#> 5 Flusight-baseline 2022-11-19     wk inc flu hosp       0 25       2022-11-19      quantile    0.75              57
#> 6 Flusight-baseline 2022-11-19     wk inc flu hosp       0 25       2022-11-19      quantile    0.9               71
```

[`hubExamples::forecast_oracle_output`](https://rdrr.io/pkg/hubExamples/man/forecast_data.html)
is the matching oracle output.

``` r

head(hubExamples::forecast_oracle_output)
#> # A tibble: 6 × 6
#>   location target_end_date target          output_type output_type_id oracle_value
#>   <chr>    <date>          <chr>           <chr>       <chr>                 <dbl>
#> 1 US       2022-10-22      wk inc flu hosp quantile    NA                     2380
#> 2 01       2022-10-22      wk inc flu hosp quantile    NA                      141
#> 3 02       2022-10-22      wk inc flu hosp quantile    NA                        3
#> 4 04       2022-10-22      wk inc flu hosp quantile    NA                       22
#> 5 05       2022-10-22      wk inc flu hosp quantile    NA                       50
#> 6 06       2022-10-22      wk inc flu hosp quantile    NA                      124
```

The main entry point is
[`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md).
It accepts a `model_out_tbl` of a **single** output type at a time,
transforms it into a `scoringutils` forecast object, computes scores
using metrics appropriate for that output type, and (by default)
summarizes the per-row scores by model.

> **Note:** While `model_out_tbl` must be filtered to a single
> `output_type` per scoring call, `oracle_output` can contain rows for
> multiple `output_type`s. The same full `oracle_output` can be passed
> to every scoring call.

## Quantile forecasts

Quantile forecasts are the most common output type in current hubverse
hubs. To score them, filter the `model_out_tbl` to
`output_type == "quantile"` and pass it to
[`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md):

``` r

quantile_out <- hubExamples::forecast_outputs |>
  dplyr::filter(output_type == "quantile")

quantile_scores <- score_model_out(
  model_out_tbl = quantile_out,
  oracle_output = hubExamples::forecast_oracle_output
)
quantile_scores
#> # A tibble: 3 × 9
#>   model_id            wis overprediction underprediction dispersion   bias interval_coverage_50 interval_coverage_90 ae_median
#>   <chr>             <dbl>          <dbl>           <dbl>      <dbl>  <dbl>                <dbl>                <dbl>     <dbl>
#> 1 Flusight-baseline  329.           79.2            233.       17.0 -0.375                0                    0.125      402.
#> 2 MOBS-GLEAM_FLUH    315.           13.6            264        37.7 -0.619                0.25                 0.562      416.
#> 3 PSI-DICE           228.           19.3            177.       32.0 -0.538                0.375                0.5        277
```

By default,
[`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md)
returns the metrics that `scoringutils` considers appropriate for the
given output type. For quantile forecasts, these are `wis`,
`overprediction`, `underprediction`, `dispersion`, `bias`,
`interval_coverage_50`, `interval_coverage_90`, `ae_median`. See
[`?score_model_out`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md)
for short descriptions of each metric and the defaults for the other
output types.

To compute only a subset, name them via the `metrics` argument (more in
[Selecting a subset of metrics](#selecting-a-subset-of-metrics) below):

``` r

score_model_out(
  model_out_tbl = quantile_out,
  oracle_output = hubExamples::forecast_oracle_output,
  metrics = c("wis", "ae_median")
)
#> # A tibble: 3 × 3
#>   model_id            wis ae_median
#>   <chr>             <dbl>     <dbl>
#> 1 Flusight-baseline  329.      402.
#> 2 MOBS-GLEAM_FLUH    315.      416.
#> 3 PSI-DICE           228.      277
```

### Custom interval coverage levels

To request interval coverage at different levels, name them explicitly
via the `metrics` argument (covered in general in [Selecting a subset of
metrics](#selecting-a-subset-of-metrics) below). `interval_coverage_XX`
reports coverage of the **central XX% interval**, which is bounded by
the `(100 - XX) / 200` and `1 - (100 - XX) / 200` quantiles. Both bounds
must be present in the data.

A common gotcha: having the `0.95` quantile in the data is not enough to
compute `interval_coverage_95`. The 95% central interval is bounded by
the 0.025 and 0.975 quantiles; the 0.95 quantile is the upper bound of
the **90%** central interval (paired with 0.05). The example hub
provides the 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, and 0.95 quantiles, so
only the following interval coverage levels are computable:

| `interval_coverage_XX` | Lower quantile | Upper quantile |
|------------------------|----------------|----------------|
| `interval_coverage_50` | 0.25           | 0.75           |
| `interval_coverage_80` | 0.10           | 0.90           |
| `interval_coverage_90` | 0.05           | 0.95           |

``` r

score_model_out(
  model_out_tbl = quantile_out,
  oracle_output = hubExamples::forecast_oracle_output,
  metrics = c("wis", "interval_coverage_80")
)
#> # A tibble: 3 × 3
#>   model_id            wis interval_coverage_80
#>   <chr>             <dbl>                <dbl>
#> 1 Flusight-baseline  329.                  0  
#> 2 MOBS-GLEAM_FLUH    315.                  0.5
#> 3 PSI-DICE           228.                  0.5
```

## Point forecasts (mean and median)

The hubverse defines two point forecast output types: `mean` and
`median`.
[`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md)
scores `mean` output types with squared error (`se_point`) and `median`
output types with absolute error (`ae_point`). The pairing follows from
a property of these metrics ([Gneiting
2011](https://doi.org/10.1198/jasa.2011.r10138)):

- The expected squared error of a prediction against draws from a
  distribution F is minimised when the prediction equals the **mean** of
  F.
- The expected absolute error is minimised when the prediction equals
  the **median** of F.

Squared error therefore directly measures how close a `mean` forecast is
to the true mean of the data, and absolute error does the same for the
`median`. A mismatched pairing (e.g. scoring `mean` output types with
absolute error) would measure how close the model’s reported mean is to
the true median rather than the true mean.

``` r

mean_out <- hubExamples::forecast_outputs |>
  dplyr::filter(output_type == "mean")

score_model_out(
  model_out_tbl = mean_out,
  oracle_output = hubExamples::forecast_oracle_output
)
#> # A tibble: 3 × 2
#>   model_id          se_point
#>   <chr>                <dbl>
#> 1 Flusight-baseline  249988.
#> 2 MOBS-GLEAM_FLUH    307063.
#> 3 PSI-DICE           142201.
```

``` r

median_out <- hubExamples::forecast_outputs |>
  dplyr::filter(output_type == "median")

score_model_out(
  model_out_tbl = median_out,
  oracle_output = hubExamples::forecast_oracle_output
)
#> # A tibble: 3 × 2
#>   model_id          ae_point
#>   <chr>                <dbl>
#> 1 Flusight-baseline     402.
#> 2 MOBS-GLEAM_FLUH       416.
#> 3 PSI-DICE              277
```

## Probability mass function (pmf) forecasts

PMF forecasts assign a probability to each category of a categorical
target. The example hub has a `wk flu hosp rate category` target with
four categories: `low`, `moderate`, `high`, `very high`.

The categories carry a natural ordering (severity), so scoring rules
that exploit that ordering give a more informative picture than scoring
rules that treat the categories as unordered labels.
[`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md)
switches between nominal (no ordering) and ordinal (with ordering)
scoring based on whether the `output_type_id_order` argument is
provided.

### Nominal scoring (no category order)

``` r

pmf_out <- hubExamples::forecast_outputs |>
  dplyr::filter(output_type == "pmf")
```

Without `output_type_id_order`, the forecast is treated as nominal. The
default metric set is `log_score`. Nominal scoring does not use any
ordering between categories: any wrong category is “equally” wrong,
weighted only by the probability placed on the true one.

``` r

score_model_out(
  model_out_tbl = pmf_out,
  oracle_output = hubExamples::forecast_oracle_output
)
#> # A tibble: 3 × 2
#>   model_id          log_score
#>   <chr>                 <dbl>
#> 1 Flusight-baseline       Inf
#> 2 MOBS-GLEAM_FLUH         Inf
#> 3 PSI-DICE                Inf
```

Every model scores `Inf`. This is the proper scoring rule behaving
correctly: each of these models assigns probability `0` to some category
that is then observed, giving `log_score = -log(0) = Inf` on that row,
and averaging propagates the `Inf` through to the model’s overall score.
The result makes cross-model comparison impossible whenever any single
row hits a zero. A common workaround, used by FluSight ([Reich et
al. 2019](https://doi.org/10.1371/journal.pcbi.1007486)), is to cap
`log_score` at a finite value before aggregation; FluSight truncates at
`-10` under the `log(p)` convention, which corresponds to capping
`log_score = -log(p)` at `10` in the scoringutils convention used here.
The cap is a domain judgement and the resulting score is no longer a
proper scoring rule. Apply the cap manually using `summarize = FALSE` to
expose per-row scores (covered in [Summarising
scores](#summarising-scores) below):

``` r

score_model_out(
  model_out_tbl = pmf_out,
  oracle_output = hubExamples::forecast_oracle_output,
  summarize = FALSE
) |>
  dplyr::mutate(log_score = pmin(log_score, 10)) |>
  dplyr::group_by(model_id) |>
  dplyr::summarise(log_score = mean(log_score), .groups = "drop")
#> # A tibble: 3 × 2
#>   model_id          log_score
#>   <chr>                 <dbl>
#> 1 Flusight-baseline      4.19
#> 2 MOBS-GLEAM_FLUH        3.97
#> 3 PSI-DICE               3.51
```

### Ordinal scoring (with category order)

Providing `output_type_id_order` declares the order of the categories
from lowest to highest.
[`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md)
then scores the forecasts as ordinal, with the default metric set
`log_score`, `rps`. Ordinal scoring uses the declared category order, so
a probability mass placed one category away from the truth is penalised
less than one placed several categories away. A forecaster who is
“nearly right” on the severity scale will therefore score better under
ordinal scoring than under nominal scoring, which treats any wrong
category as equally wrong.

The category order is declared in the hub’s `tasks.json` config (under
`output_type.pmf.output_type_id.required` for the relevant model task).
For this hub, the order is `low`, `moderate`, `high`, `very high`:

``` r

score_model_out(
  model_out_tbl = pmf_out,
  oracle_output = hubExamples::forecast_oracle_output,
  output_type_id_order = c("low", "moderate", "high", "very high")
)
#> # A tibble: 3 × 3
#>   model_id          log_score   rps
#>   <chr>                 <dbl> <dbl>
#> 1 Flusight-baseline       Inf 0.835
#> 2 MOBS-GLEAM_FLUH         Inf 0.720
#> 3 PSI-DICE                Inf 0.679
```

`log_score` runs into the same `Inf` issue described above; `rps` is
finite even when the model assigns `0` probability to the observed
category, so it remains comparable across models without any correction.
The same manual cap workaround applies to `log_score` here.

If a hub has pmf targets with different bin sets (for example, one
target with three categories and another with five), score them in
separate calls so that each call has a single coherent category set.

## Sample forecasts

Sample-based forecasts represent the predictive distribution as a set of
draws. The hubverse supports two scoring modes:

- **Marginal**: each modeling task (each combination of task IDs) is
  scored independently. This is appropriate when the inferential target
  is the marginal predictive distribution at each task ID.
- **Compound**: samples are scored jointly across a subset of task ID
  dimensions, treating each sample draw as a joint prediction over those
  dimensions. This is appropriate when the inferential target is the
  joint distribution (for example, a trajectory over horizons).

### Marginal sample scoring

By default,
[`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md)
scores sample forecasts marginally. The default metric set is `bias`,
`dss`, `crps`, `overprediction`, `underprediction`, `dispersion`,
`log_score`, `mad`, `ae_median`, `se_mean`, headlined by the [continuous
ranked probability score
(CRPS)](https://epiforecasts.io/scoringutils/reference/crps_sample.html).
Below we restrict the output to just CRPS for readability, using the
`metrics` argument covered in [Selecting a subset of
metrics](#selecting-a-subset-of-metrics) below:

``` r

score_model_out(
  model_out_tbl = sample_out,
  oracle_output = hubExamples::forecast_oracle_output,
  metrics = "crps"
)
#> # A tibble: 3 × 2
#>   model_id           crps
#>   <chr>             <dbl>
#> 1 Flusight-baseline  352.
#> 2 MOBS-GLEAM_FLUH    347.
#> 3 PSI-DICE           247.
```

### Compound sample scoring

To score samples jointly, supply `compound_taskid_set`: the task ID
columns that stay constant within a single sample draw. The remaining
task ID columns vary within a draw and are scored jointly using
multivariate scoring rules: the default metric set is `energy_score`,
`variogram_score`.

The `compound_taskid_set` is declared in the hub’s `tasks.json` config
(under `output_type.sample.output_type_id_params.compound_taskid_set`).
For the example hub, each sample draw spans all horizons for a given
`reference_date` and `location`, so the `compound_taskid_set` is
`c("reference_date", "location")`:

``` r

score_model_out(
  model_out_tbl = sample_out,
  oracle_output = hubExamples::forecast_oracle_output,
  compound_taskid_set = c("reference_date", "location")
)
#> # A tibble: 3 × 3
#>   model_id          energy_score variogram_score
#>   <chr>                    <dbl>           <dbl>
#> 1 Flusight-baseline         773.           1524.
#> 2 MOBS-GLEAM_FLUH           811.           1695.
#> 3 PSI-DICE                  571.           1264.
```

[`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md)
validates that the submitted samples are compatible with the requested
`compound_taskid_set` and errors with a clear message if they are not.

## Selecting a subset of metrics

The `metrics` argument applies across all output types: pass any subset
of the metric names valid for the relevant output type to compute only
those. For quantile forecasts, for example:

``` r

score_model_out(
  model_out_tbl = quantile_out,
  oracle_output = hubExamples::forecast_oracle_output,
  metrics = c("wis", "ae_median")
)
#> # A tibble: 3 × 3
#>   model_id            wis ae_median
#>   <chr>             <dbl>     <dbl>
#> 1 Flusight-baseline  329.      402.
#> 2 MOBS-GLEAM_FLUH    315.      416.
#> 3 PSI-DICE           228.      277
```

## Summarising scores

By default,
[`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md)
returns one row per model, with each metric averaged across all task-ID
combinations. Two arguments control this behaviour for any output type.

### Per-row scores

Setting `summarize = FALSE` returns one score per row of the input
`model_out_tbl`, with no aggregation across task-ID combinations:

``` r

score_model_out(
  model_out_tbl = quantile_out,
  oracle_output = hubExamples::forecast_oracle_output,
  summarize = FALSE
) |>
  head()
#> # A tibble: 6 × 14
#>   model_id          reference_date target          horizon location target_end_date   wis overprediction underprediction dispersion  bias interval_coverage_50 interval_coverage_90 ae_median
#>   <chr>             <date>         <chr>             <int> <chr>    <date>          <dbl>          <dbl>           <dbl>      <dbl> <dbl> <lgl>                <lgl>                    <dbl>
#> 1 Flusight-baseline 2022-11-19     wk inc flu hosp       0 25       2022-11-19       15.4              0            12.6       2.83  -0.9 FALSE                TRUE                        28
#> 2 Flusight-baseline 2022-11-19     wk inc flu hosp       1 25       2022-11-26      149.               0           145.        4.89  -1   FALSE                FALSE                      170
#> 3 Flusight-baseline 2022-11-19     wk inc flu hosp       2 25       2022-12-03      370.               0           363.        6.3   -1   FALSE                FALSE                      395
#> 4 Flusight-baseline 2022-11-19     wk inc flu hosp       3 25       2022-12-10      498.               0           490.        7.37  -1   FALSE                FALSE                      527
#> 5 Flusight-baseline 2022-11-19     wk inc flu hosp       0 48       2022-11-19      116.               0           103.       12.4   -1   FALSE                FALSE                      178
#> 6 Flusight-baseline 2022-11-19     wk inc flu hosp       1 48       2022-11-26      788.               0           768.       20.0   -1   FALSE                FALSE                      877
```

### Custom grouping

A custom `by` argument summarises by a different grouping, for example
each model’s scores by horizon:

``` r

score_model_out(
  model_out_tbl = quantile_out,
  oracle_output = hubExamples::forecast_oracle_output,
  by = c("model_id", "horizon")
)
#> # A tibble: 12 × 10
#>    model_id          horizon   wis overprediction underprediction dispersion   bias interval_coverage_50 interval_coverage_90 ae_median
#>    <chr>               <int> <dbl>          <dbl>           <dbl>      <dbl>  <dbl>                <dbl>                <dbl>     <dbl>
#>  1 Flusight-baseline       0 117.            57.5           51.1        8.84 -0.475                 0                    0.25     162. 
#>  2 Flusight-baseline       1 384.           105.           264.        14.9  -0.5                   0                    0        450  
#>  3 Flusight-baseline       2 414.            42.0          351.        20.2  -0.5                   0                    0        498. 
#>  4 Flusight-baseline       3 402.           112.           266.        24.2  -0.025                 0                    0.25     498. 
#>  5 MOBS-GLEAM_FLUH         0  80.5           29.9           11.6       39.1  -0.3                   0.5                  0.75     172  
#>  6 MOBS-GLEAM_FLUH         1 292.            24.4          224.        44.3  -0.425                 0.25                 0.5      396  
#>  7 MOBS-GLEAM_FLUH         2 451.             0            413.        37.5  -0.9                   0                    0.5      555. 
#>  8 MOBS-GLEAM_FLUH         3 437.             0            407.        29.8  -0.85                  0.25                 0.5      543. 
#>  9 PSI-DICE                0  65.0           36.6            4.18      24.3  -0.125                 0.75                 0.75      97.8
#> 10 PSI-DICE                1 248.            40.5          182.        25.6  -0.45                  0                    0.25     336. 
#> 11 PSI-DICE                2 322.             0            289.        33.3  -0.825                 0.25                 0.5      386  
#> 12 PSI-DICE                3 276.             0            231.        45.1  -0.75                  0.5                  0.5      288.
```

## Relative skill scores

Specifying `relative_metrics` adds a column expressing each model’s
score for that metric relative to the rest of the model set.
`relative_metrics` should only be set to positive-valued,
lower-is-better metrics. Proper scoring rules like `wis`, `crps`,
`log_score`, `rps`, `ae_point`, `se_point`, `ae_median` are the natural
choices; the WIS decomposition components `overprediction`,
`underprediction`, and `dispersion` also work mathematically, though
`dispersion` should be interpreted with care (a smaller ratio means a
sharper forecast, which is only beneficial if the model is calibrated).
Interval coverage metrics and `bias` are rejected outright by
[`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md):
interval coverage targets a nominal level rather than a “lower is
better” direction, and `bias` is a signed quantity for which the
geometric mean of pairwise ratios is undefined.

By default, the column is named `<metric>_relative_skill` and holds the
geometric mean of pairwise score ratios across all other models. Values
below 1 indicate a model that scored lower on average than its
opponents.

``` r

score_model_out(
  model_out_tbl = quantile_out,
  oracle_output = hubExamples::forecast_oracle_output,
  metrics = c("wis", "interval_coverage_80", "interval_coverage_90"),
  relative_metrics = "wis"
)
#> # A tibble: 3 × 5
#>   model_id            wis interval_coverage_80 interval_coverage_90 wis_relative_skill
#>   <chr>             <dbl>                <dbl>                <dbl>              <dbl>
#> 1 Flusight-baseline  329.                  0                  0.125              1.15 
#> 2 MOBS-GLEAM_FLUH    315.                  0.5                0.562              1.10 
#> 3 PSI-DICE           228.                  0.5                0.5                0.794
```

Providing a `baseline` rescales the relative skill so that the chosen
baseline model has a skill of 1, and returns it in a column named
`<metric>_scaled_relative_skill`. Other models’ values are then
interpretable directly with respect to the baseline (e.g. 0.7 means “30%
lower WIS than the baseline on average”):

``` r

score_model_out(
  model_out_tbl = quantile_out,
  oracle_output = hubExamples::forecast_oracle_output,
  metrics = c("wis", "interval_coverage_80", "interval_coverage_90"),
  relative_metrics = "wis",
  baseline = "Flusight-baseline"
)
#> # A tibble: 3 × 6
#>   model_id            wis interval_coverage_80 interval_coverage_90 wis_relative_skill wis_scaled_relative_skill
#>   <chr>             <dbl>                <dbl>                <dbl>              <dbl>                     <dbl>
#> 1 Flusight-baseline  329.                  0                  0.125              1.15                      1    
#> 2 MOBS-GLEAM_FLUH    315.                  0.5                0.562              1.10                      0.957
#> 3 PSI-DICE           228.                  0.5                0.5                0.794                     0.692
```

Relative-skill computation requires that `"model_id"` is included in
`by`, which is the default.

## Scale transformations

Quantile, mean, median, and sample forecasts can be scored on a
transformed scale, which is useful when forecasts span several orders of
magnitude or when an exponential-growth process is better assessed on
the log scale. Pass a transformation function to the `transform`
argument:

``` r

score_model_out(
  model_out_tbl = quantile_out,
  oracle_output = hubExamples::forecast_oracle_output,
  metrics = "wis",
  transform = scoringutils::log_shift,
  offset = 1
)
#> # A tibble: 3 × 2
#>   model_id            wis
#>   <chr>             <dbl>
#> 1 Flusight-baseline 0.540
#> 2 MOBS-GLEAM_FLUH   0.574
#> 3 PSI-DICE          0.361
```

[`scoringutils::log_shift()`](https://epiforecasts.io/scoringutils/reference/log_shift.html)
is recommended for log transformations because the `offset` argument
handles zeros gracefully; using
[`log()`](https://rdrr.io/r/base/Log.html) directly on data that contain
zeros produces `-Inf` and propagates `NaN` through the scores.

To compare natural-scale and transformed-scale scores side by side, set
`transform_append = TRUE`. The result has a `scale` column
distinguishing rows scored on each scale:

``` r

score_model_out(
  model_out_tbl = quantile_out,
  oracle_output = hubExamples::forecast_oracle_output,
  metrics = "wis",
  transform = scoringutils::log_shift,
  transform_append = TRUE,
  offset = 1
)
#> # A tibble: 6 × 3
#>   model_id          scale         wis
#>   <chr>             <chr>       <dbl>
#> 1 Flusight-baseline natural   329.   
#> 2 MOBS-GLEAM_FLUH   natural   315.   
#> 3 PSI-DICE          natural   228.   
#> 4 Flusight-baseline log_shift   0.540
#> 5 MOBS-GLEAM_FLUH   log_shift   0.574
#> 6 PSI-DICE          log_shift   0.361
```

### Custom transform labels

When `transform_append = TRUE`, the `scale` column labels the
transformed rows using a name inferred from the `transform` function
(e.g. `"log_shift"` for
[`scoringutils::log_shift`](https://epiforecasts.io/scoringutils/reference/log_shift.html),
`"sqrt"` for `sqrt`). Override the inferred label by passing
`transform_label` explicitly, for example to shorten `"log_shift"` to
`"log"`:

``` r

score_model_out(
  model_out_tbl = quantile_out,
  oracle_output = hubExamples::forecast_oracle_output,
  metrics = "wis",
  transform = scoringutils::log_shift,
  transform_label = "log",
  transform_append = TRUE,
  offset = 1
)
#> # A tibble: 6 × 3
#>   model_id          scale       wis
#>   <chr>             <chr>     <dbl>
#> 1 Flusight-baseline natural 329.   
#> 2 MOBS-GLEAM_FLUH   natural 315.   
#> 3 PSI-DICE          natural 228.   
#> 4 Flusight-baseline log       0.540
#> 5 MOBS-GLEAM_FLUH   log       0.574
#> 6 PSI-DICE          log       0.361
```

For anonymous transformations, `transform_label` is **required**,
because there is no function name to infer from:

``` r

score_model_out(
  model_out_tbl = mean_out,
  oracle_output = hubExamples::forecast_oracle_output,
  transform = function(x) sqrt(x),
  transform_label = "sqrt",
  transform_append = TRUE
)
#> # A tibble: 6 × 3
#>   model_id          scale   se_point
#>   <chr>             <chr>      <dbl>
#> 1 Flusight-baseline natural 249988. 
#> 2 MOBS-GLEAM_FLUH   natural 307063. 
#> 3 PSI-DICE          natural 142201. 
#> 4 Flusight-baseline sqrt        72.0
#> 5 MOBS-GLEAM_FLUH   sqrt        88.0
#> 6 PSI-DICE          sqrt        37.2
```

Interval coverage metrics are invariant under monotonic transforms (the
0.25 quantile is still the 0.25 quantile after a log transform, so
whether the observation falls inside a central interval is unchanged),
so requesting them alongside a `transform` yields the same values on the
transformed scale as on the natural scale.

Scale transformations are not supported for `pmf` output types.

## Where to go next

- [`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md)
  is a convenience that bundles the hubverse-to-scoringutils
  transformation with the scoring step. If you need the intermediate
  scoringutils forecast object (for example, to use `scoringutils`
  plotting helpers or to compose custom scoring metrics), the
  [`transform_quantile_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/transform_quantile_model_out.md),
  [`transform_point_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/transform_point_model_out.md),
  [`transform_pmf_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/transform_pmf_model_out.md),
  and
  [`transform_sample_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/transform_sample_model_out.md)
  exported helpers produce it directly.
- See
  [`?score_model_out`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md)
  for the full argument reference and a list of available metrics per
  output type.
- The [`scoringutils`](https://epiforecasts.io/scoringutils/) package
  provides rich tooling for visualizing and further analyzing the score
  tibbles produced by
  [`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md),
  including
  [`plot_wis()`](https://epiforecasts.io/scoringutils/reference/plot_wis.html)
  and
  [`plot_pairwise_comparison()`](https://epiforecasts.io/scoringutils/reference/plot_pairwise_comparison.html).
- The [`hubExamples`](https://hubverse-org.github.io/hubExamples/)
  package contains additional example data sets for experimenting with
  hubEvals.
