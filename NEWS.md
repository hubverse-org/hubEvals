# hubEvals (development version)

* Add support for scoring sample output types via `transform_sample_model_out()` and the `"sample"` case in `score_model_out()`. Marginal scoring produces metrics such as CRPS, bias, and DSS; compound scoring (via the new `compound_taskid_set` argument) produces multivariate scores such as energy score and variogram score for joint forecasts (#94).

* `score_model_out()` now errors with a clear message when a scale transformation produces non-finite values (NaN or Inf), instead of silently returning invalid scores (#99).

# hubEvals 0.1.0

* Add `transform`, `transform_append`, and `transform_label` arguments to `score_model_out()` for computing scores on transformed scales (e.g., log, sqrt). Supported for quantile, mean, and median output types (#48, #91).

# hubEvals 0.0.1

* Export functions `transform_pmf_model_out()`, `transform_point_model_out()`, and `transform_quantile_model_out()` used to transform hubverse model outputs into a `scoringutils` forecast object
* Update package dependencies to use CRAN releases when available
* Update README to include simple examples of package functions

# hubEvals 0.0.0.9001

* Add `score_model_out()` function for evaluating model outputs
* Add tests to package
* Update organisation name to hubverse-org

# hubEvals 0.0.0.9000

* Initial package dev setup.
