## code to prepare `example_model_outputs` dataset

library(hubUtils)
library(distfromq)

hub_path <- "data-raw/example-simple-forecast-hub"
example_model_output <- hubUtils::connect_hub(hub_path) |>
  dplyr::collect()

q_lvls_keep <- c("0.050", "0.100", "0.250", "0.500", "0.750", "0.900", "0.950")
example_quantile_model_output <- example_model_output |>
  dplyr::filter(
    horizon %in% c(0, 7),
    location %in% c("25", "US"),
    output_type == "quantile",
    format(output_type_id, nsmall = 3) %in% q_lvls_keep
  )

usethis::use_data(example_quantile_model_output, overwrite = TRUE)


example_median_model_output <- example_model_output |>
  dplyr::filter(
    horizon %in% c(0, 7),
    location %in% c("25", "US"),
    output_type == "quantile",
    format(output_type_id, nsmall = 3) == "0.500"
  ) |>
  dplyr::mutate(
    output_type = "median",
    output_type_id = "NA"
  )

usethis::use_data(example_median_model_output, overwrite = TRUE)


set.seed(42)
example_mean_model_output <- example_model_output |>
  dplyr::filter(
    horizon %in% c(0, 7),
    location %in% c("25", "US"),
    output_type == "quantile"
  ) |>
  dplyr::group_by(origin_date, horizon, location, target, model_id) |>
  dplyr::summarize(
    value = distfromq::make_r_fn(ps = output_type_id, qs = value)(1e5) |>
      mean(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    output_type = "mean",
    output_type_id = "NA"
  )

usethis::use_data(example_mean_model_output, overwrite = TRUE)
