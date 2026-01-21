# hubEvals (development version)

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
