#' WIS Ranking
#'
#' @param wis_data
#'
#' @return
#' @export
#'
#' @examples

wis_ranking_baseline = function(wis_data){

  require(dplyr)

  unique_models = unique(wis_data$model)
  uniquetargets <- length(unique(wis_data$target_end_date))
  uniquelocations <- length(unique(wis_data$location_name))

  ranking_baseline <- data.frame(model = character(), mean.WIS = numeric(), rel.WIS.skill = numeric(), MAE = numeric(),
                                 Percent.Cov.50 = numeric(),Percent.Cov.95 = numeric(), frac.forecasts.submitted = numeric(),
                                 frac.locations.submitted = numeric(), frac.locations.fully.forecasted = numeric(),
                                 frac.submitted.locations.fully.forecasted = numeric())

  for (i in 1:length(unique_models)){
    step0 = wis_data %>% filter(model == unique_models[i])

    locations.fully.submitted = step0 %>% group_by(location_name) %>%
      summarise(subs = n()) %>% ungroup() %>%
      nrow()

    step1 = step0 %>% summarise(model = unique(model),
                                mean.WIS = mean(WIS),
                                MAE = mean(abs.error, na.rm = TRUE),
                                Percent.Cov.50 = mean(coverage.50, na.rm = TRUE),
                                Percent.Cov.95 = mean(coverage.95, na.rm = TRUE))

    step2 = wis_data %>% filter(model == unique_models[i]) %>%
      summarise(model = unique(model))

    step3 = left_join(step1, step2, by = "model") %>%
      mutate(frac.locations.submitted = length(unique(step0$location_name))/uniquelocations,
             frac.locations.fully.forecasted = locations.fully.submitted/uniquelocations,
             frac.submitted.locations.fully.forecasted = frac.locations.fully.forecasted/frac.locations.submitted
      ) %>%
      select(model, mean.WIS, MAE, Percent.Cov.50, Percent.Cov.95, frac.locations.submitted,
             frac.locations.fully.forecasted, frac.submitted.locations.fully.forecasted)

    ranking_baseline <- rbind(ranking_baseline, step3)
  }
  return(ranking_baseline)
}
