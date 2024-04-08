#' Title
#'
#' @param wis_data
#'
#' @return
#' @export
#'
#' @examples

wis_ranking_location <- function(wis_data){

  require(dplyr)

  df <- wis_data

  unique_models = unique(df$model)
  uniquetargets <- length(unique(df$target_end_date))
  uniquelocations <- length(unique(df$location_name))

  ranking_baseline <- data.frame(model = character(), location_name = character(), mean.WIS = numeric(), MAE = numeric(),
                                 Percent.Cov.50 = numeric(),Percent.Cov.95 = numeric(), relative_WIS = numeric())

  for (i in 1:length(unique_models)){
    step0 = df %>% filter(model == unique_models[i])

    step1 = step0 %>% group_by(location_name) %>% summarise(model = unique(model),
                                                            mean.WIS = mean(WIS, na.rm = TRUE),
                                                            MAE = mean(abs.error, na.rm = TRUE),
                                                            Percent.Cov.50 = mean(coverage.50, na.rm = TRUE),
                                                            Percent.Cov.95 = mean(coverage.95, na.rm = TRUE))

    step2 = wis_data %>% filter(model == unique_models[i]) %>%
                                  group_by(location_name) %>%
                                  summarise(model = unique(model))

    step3 = left_join(step1, step2, by = c("model", "location_name")) %>%
      # mutate(frac.forecasts.submitted =
      #          nrow(filter(df, model == unique_models[i]))/nrow(filter(df, model == "Flusight-baseline")),
      #        frac.locations.submitted = length(unique(step0$location_name))/uniquelocations,
      #        frac.locations.fully.forecasted = locations.fully.submitted/uniquelocations,
      #        frac.submitted.locations.fully.forecasted = frac.locations.fully.forecasted/frac.locations.submitted
      #        ) %>%
      select(model, location_name, mean.WIS, MAE, Percent.Cov.50, Percent.Cov.95)

    ranking_baseline <- rbind(ranking_baseline, step3)
  }

  return(ranking_baseline)

}
