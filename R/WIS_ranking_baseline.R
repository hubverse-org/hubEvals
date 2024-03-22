#' WIS Ranking
#'
#' @param df A data frame
#'
#' @return
#' @export
#'
#' @examples

WIS_ranking_baseline = function(df){
    unique_models = unique(df$model)
    uniquetargets <- length(unique(df$target_end_date))
    uniquelocations <- length(unique(df$location_name))

    ranking_baseline <- data.frame(model = character(), mean.WIS = numeric(), rel.WIS.skill = numeric(), MAE = numeric(),
                                   Percent.Cov.50 = numeric(),Percent.Cov.95 = numeric(), frac.forecasts.submitted = numeric(),
                                   frac.locations.submitted = numeric(), frac.locations.fully.forecasted = numeric(),
                                   frac.submitted.locations.fully.forecasted = numeric())

    for (i in 1:length(unique_models)){
        step0 = df %>% filter(model == unique_models[i])

        locations.fully.submitted = step0 %>% group_by(location_name) %>%
            summarise(subs = n()) %>% ungroup() %>%
            #filter(subs >= 2*uniquetargets) %>% #possibly update if definition of fully submitted changes
            nrow()

        step1 = step0 %>% summarise(model = unique(model),
                                    mean.WIS = mean(WIS),
                                    MAE = mean(abs.error, na.rm = TRUE),
                                    Percent.Cov.50 = mean(coverage.50, na.rm = TRUE),
                                    Percent.Cov.95 = mean(coverage.95, na.rm = TRUE))

        step2 = left_join(x = filter(df, model == unique_models[i]), y = filter(df, model == contains("baseline")),
                          by = c("location", "target", "target_end_date")) %>%
            summarise(model = unique(model.x),
                      rel.WIS.skill = mean(WIS.x)/mean(WIS.y))

        step3 = left_join(step1, step2, by = "model") %>%
            mutate(frac.forecasts.submitted =
                       nrow(filter(df, model == unique_models[i]))/nrow(filter(df, model == contains("baseline"))),
                   frac.locations.submitted = length(unique(step0$location_name))/uniquelocations,
                   frac.locations.fully.forecasted = locations.fully.submitted/uniquelocations,
                   frac.submitted.locations.fully.forecasted = frac.locations.fully.forecasted/frac.locations.submitted
            ) %>%
            select(model, mean.WIS, rel.WIS.skill, MAE, Percent.Cov.50, Percent.Cov.95,
                   frac.forecasts.submitted, frac.locations.submitted,
                   frac.locations.fully.forecasted, frac.submitted.locations.fully.forecasted)

        ranking_baseline <- rbind(ranking_baseline, step3)
    }
    return(ranking_baseline)
}
