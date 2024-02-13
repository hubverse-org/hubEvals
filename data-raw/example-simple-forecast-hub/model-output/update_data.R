library(dplyr)

files <- Sys.glob("model-output/*/*")

for (f in files) {
  df <- read.csv(f)
  df <- df %>%
    dplyr::transmute(
      origin_date = dplyr::case_when(
        # forecast_date %in% c("2022-11-27", "2022-11-28") ~ "2022-11-28",
        # forecast_date %in% c("2022-12-04", "2022-12-05") ~ "2022-12-05",
        # forecast_date %in% c("2022-12-11", "2022-12-12") ~ "2022-12-12"
        forecast_date %in% c("2022-11-27", "2022-11-28") ~ "2022-12-05",
        forecast_date %in% c("2022-12-04", "2022-12-05") ~ "2022-12-12",
        forecast_date %in% c("2022-12-11", "2022-12-12") ~ "2022-12-19"
      ),
      horizon = as.integer(stringr::str_split_i(df$target, " ", 1)) - 7,
      location = location,
      target = "inc covid hosp",
      output_type = type,
      output_type_id = quantile,
      value = round(value)
    ) %>%
    dplyr::filter(
      location %in% c("US", "25", "20"),
      horizon <= 14,
      output_type == "quantile"
    )

  df <- dplyr::bind_rows(
    dplyr::mutate(df, output_type_id = as.character(output_type_id)),
    dplyr::filter(df, abs(output_type_id - 0.5) < 0.0001) %>%
      dplyr::mutate(
        output_type = "median",
        output_type_id = "NA"
      )
  )

  write.csv(df, file = f, row.names = FALSE)
}
