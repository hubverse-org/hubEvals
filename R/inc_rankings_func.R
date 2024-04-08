#' Incidence rankings
#'
#' @param wis_data Data frame with WIS scoring
#'
#' @return
#' @export
#'
#' @examples


inc_rankings_func <- function(wis_data){

  require(magrittr)

  rankings = wis_ranking_baseline(wis_data) %>%
    # select(-rank) %>%
    # dplyr::filter(frac.forecasts.submitted >= 0.75) %>%
    dplyr::mutate(model = as.character(model),
           mean.WIS = round(mean.WIS,2),
           #rel.WIS.skill = round(rel.WIS.skill,2),
           MAE = round(MAE,2),
           Percent.Cov.50 = round(100*Percent.Cov.50),
           Percent.Cov.95 = round(100*Percent.Cov.95),
           #frac.forecasts.submitted = round(100*frac.forecasts.submitted),
           frac.locations.fully.forecasted = round(100*frac.locations.fully.forecasted),
           frac.submitted.locations.fully.forecasted = round(100*frac.submitted.locations.fully.forecasted)) %>%
    dplyr::arrange(mean.WIS)
  return(rankings)
}
