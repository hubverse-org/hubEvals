## code to prepare `example_hub_targets` dataset
## note: `example_hub_outputs` should be created first
library(hubUtils)

load("data/example_model_outputs.rda")

hub_path <- "data-raw/example-simple-forecast-hub"
target_data_path <- file.path(hub_path, "target-data",
                              "covid-hospitalizations.csv")

raw_target_data <- read.csv(target_data_path)
raw_target_data <- raw_target_data |>
  dplyr::rename(target_date = time_idx)

example_model_outputs <- example_model_outputs |>
  dplyr::mutate(target_date = as.character(origin_date + horizon))

example_hub_targets <- example_model_outputs |>
  dplyr::distinct(
    origin_date, horizon, location, target, target_date) |>
  dplyr::left_join(
    raw_target_data, by = c("location", "target_date", "target")) |>
  dplyr::select(-target_date)

head(example_hub_targets)

usethis::use_data(example_hub_targets, overwrite = TRUE)
