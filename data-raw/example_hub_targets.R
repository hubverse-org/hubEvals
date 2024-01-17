## code to prepare `example_hub_targets` dataset
## note: `example_hub_outputs` should be created first
library(hubUtils)

load("data/example_model_outputs.rda")

hub_path <- "data-raw/example-simple-forecast-hub"
target_data_path <- file.path(hub_path, "target-data",
                              "covid-hospitalizations.csv")

target_data <- read.csv(target_data_path)
target_data <- target_data |>
  dplyr::rename(target_date = time_idx)

example_model_outputs <- example_model_outputs |>
  dplyr::mutate(target_date = origin_date + horizon)

target_data |>
  dplyr::left_join(
    example_model_outputs |>
      dplyr::distinct(origin_date, horizon) |>
      dplyr::mutate(target_date = as.character(origin_date + horizon))
  )

head(target_data)

usethis::use_data(example_hub_targets, overwrite = TRUE)
