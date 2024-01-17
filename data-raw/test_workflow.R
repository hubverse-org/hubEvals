library(dplyr)

load("data/example_model_outputs.rda")

example_quantile_model_output <- example_model_outputs |>
  dplyr::filter(
    origin_date == "2022-12-05",
    model_id %in% c("UMass-ar", "UMass-gbq"),
    output_type_id %in% c(0.05, 0.25, 0.5, 0.75, 0.95),
    location %in% c("US", "20"),
    horizon %in% c(-6, 3))

example_processed_target_data <- expand.grid(
  origin_date = as.Date("2022-12-05"),
  location = c("US", "20"),
  horizon = c(-6, 3),
  target = "inc covid hosp"
) |>
  cbind(value = c(5022, 37, 5141, 59)) ## manual lookups of real data


left_join(example_quantile_model_output |> rename(predicted = value),
          example_processed_target_data |> rename(observed = value)) |>
  rename(quantile = output_type_id,
         model = model_id) |>
  select(-output_type) |>
  scoringutils::score()



