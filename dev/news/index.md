# Changelog

## hubEvals (development version)

- [`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md)
  now handles disaggregated relative-skill scoring where some comparison
  groups (the `by` columns other than `model_id`) cannot be compared,
  instead of aborting the whole call with a cryptic `scoringutils` error
  (“Baseline comparator … missing”). A group containing only one model
  is treated like the global single-model case, with relative skill
  filled as `1` (a model has skill `1` relative to itself). A group from
  which a requested `baseline` is absent has its relative and scaled
  relative skill reported as `NA`, with a warning naming the affected
  groups; the absolute scores for those groups are still returned
  unchanged. A `baseline` that is absent from the data entirely remains
  an error
  ([\#135](https://github.com/hubverse-org/hubEvals/issues/135)).

## hubEvals 0.3.0

CRAN release: 2026-06-29

- [`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md)
  now handles single-model input gracefully when relative metrics are
  requested. Previously this errored via `scoringutils` (“not enough
  comparators”); now the relative-skill columns are filled with `1`,
  matching the trivial fact that a model has skill `1` relative to
  itself. If a `baseline` is supplied that does not match the lone
  model,
  [`score_model_out()`](https://hubverse-org.github.io/hubEvals/dev/reference/score_model_out.md)
  errors with a clear message
  ([\#75](https://github.com/hubverse-org/hubEvals/issues/75)).

- New “Getting started with hubEvals” vignette walking through the main
  scoring workflows for each supported output type (quantile, mean,
  median, pmf nominal/ordinal, sample marginal/compound), the
  `relative_metrics` and `baseline` arguments for relative-skill
  scoring, and the `transform` and `transform_append` arguments for
  scoring on transformed scales
  ([\#38](https://github.com/hubverse-org/hubEvals/issues/38)).

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
