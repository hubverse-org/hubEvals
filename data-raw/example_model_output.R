## code to prepare `example_model_outputs` dataset

library(hubUtils)

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
