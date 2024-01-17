## code to prepare `example_model_outputs` dataset

library(hubUtils)

hub_path <- "data-raw/example-simple-forecast-hub"
example_model_outputs <- hubUtils::connect_hub(hub_path) %>%
  dplyr::collect()

usethis::use_data(example_model_outputs, overwrite = TRUE)
