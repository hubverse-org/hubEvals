#' Incidence rankings
#'
#' @param WIS_data Data frame with WIS scoring
#'
#' @return
#' @export
#'
#' @examples


inc_rankings_func <- function(WIS_data){
    rankings =
        make_WIS_ranking_baseline(WIS_data) %>%
        # select(-rank) %>%
        filter(frac.forecasts.submitted >= 0.75) %>%

        mutate(model = model_display_names[as.character(model)],
               mean.WIS = round(mean.WIS,2),
               rel.WIS.skill = round(rel.WIS.skill,2),
               MAE = round(MAE,2),
               Percent.Cov.50 = round(100*Percent.Cov.50),
               Percent.Cov.95 = round(100*Percent.Cov.95),
               frac.forecasts.submitted = round(100*frac.forecasts.submitted),
               frac.locations.submitted = round(100*frac.locations.submitted),
               frac.locations.fully.forecasted = round(100*frac.locations.fully.forecasted),
               frac.submitted.locations.fully.forecasted = round(100*frac.submitted.locations.fully.forecasted)) %>%
        arrange(rel.WIS.skill)
    return(rankings)
}
