# Changelog

## hubEvals (development version)

- Fix
  [`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md)
  so that requesting `transform_append = TRUE` with default
  `summarize = TRUE` now correctly returns one row per `scale` (natural
  and transformed) per model, instead of silently averaging across
  scales ([\#122](https://github.com/hubverse-org/hubEvals/issues/122)).

- [`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md)
  now errors with a clear hubEvals message when `"bias"` is requested as
  a relative metric, instead of letting `scoringutils` fail downstream
  with a cryptic “all values must have the same sign” error. Bias is a
  signed quantity, so a geometric-mean pairwise ratio has no clean
  interpretation
  ([\#119](https://github.com/hubverse-org/hubEvals/issues/119)).

- [`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md)
  now returns a tibble (inheriting from scoringutils’ `scores` class)
  instead of a `data.table`. This gives more predictable user-facing
  behaviour (e.g. with `$` access, printing, and dplyr) while keeping
  the `scores` class so downstream scoringutils helpers like
  `get_metrics()` continue to work
  ([\#70](https://github.com/hubverse-org/hubEvals/issues/70)).

- [`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md)
  now errors when no requested metric produces a score.

- Fix
  [`transform_quantile_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/transform_quantile_model_out.md),
  [`transform_point_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/transform_point_model_out.md),
  and
  [`transform_sample_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/transform_sample_model_out.md)
  to handle oracle outputs that carry an `output_type_id` column without
  an `output_type` column. Previously, this combination caused
  `as_forecast_*()` to error on a stray `output_type_id`
  ([\#73](https://github.com/hubverse-org/hubEvals/issues/73)).

## hubEvals 0.2.0

- Add support for scoring sample output types via
  [`transform_sample_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/transform_sample_model_out.md)
  and the `"sample"` case in
  [`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md).
  Marginal scoring produces metrics such as CRPS, bias, and DSS;
  compound scoring (via the new `compound_taskid_set` argument) produces
  multivariate scores such as energy score and variogram score for joint
  forecasts
  ([\#94](https://github.com/hubverse-org/hubEvals/issues/94)).

- [`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md)
  now errors with a clear message when a scale transformation produces
  non-finite values (NaN or Inf), instead of silently returning invalid
  scores ([\#99](https://github.com/hubverse-org/hubEvals/issues/99)).

## hubEvals 0.1.0

- Add `transform`, `transform_append`, and `transform_label` arguments
  to
  [`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md)
  for computing scores on transformed scales (e.g., log, sqrt).
  Supported for quantile, mean, and median output types
  ([\#48](https://github.com/hubverse-org/hubEvals/issues/48),
  [\#91](https://github.com/hubverse-org/hubEvals/issues/91)).

## hubEvals 0.0.1

- Export functions
  [`transform_pmf_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/transform_pmf_model_out.md),
  [`transform_point_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/transform_point_model_out.md),
  and
  [`transform_quantile_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/transform_quantile_model_out.md)
  used to transform hubverse model outputs into a `scoringutils`
  forecast object
- Update package dependencies to use CRAN releases when available
- Update README to include simple examples of package functions

## hubEvals 0.0.0.9001

- Add
  [`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md)
  function for evaluating model outputs
- Add tests to package
- Update organisation name to hubverse-org

## hubEvals 0.0.0.9000

- Initial package dev setup.
