#' Data frame for scoring
#'
#' @param all_dat Projection (outcome) data; all_dat must include columns: target_end_date, forecast_date, quantile, value, type, location_name, location, and age_group
#' @param obs_data "Ground truth" (observed) data; obs_dat must include columns: value_inc, target_end_date, location_name, location, and age_group
#'
#' @return Data frame with observed and projection data - ready to be scored
#' @export
#'
#' @examples
#'
#'

dat_for_scores_function <- function(all_dat, obs_data){

    all_dat = drop_na(all_dat)

    dat_for_scores = all_dat %>%
        mutate(target_end_date = as.Date(target_end_date),
               forecast_date = as.Date(forecast_date),
               value = value,
               value = case_when(quantile==0.5 ~ round(value),
                                 quantile<0.5 ~ floor(value),
                                 quantile>0.5 ~ ceiling(value)),
               submission_deadline =
                   get_next_saturday(as.Date(forecast_date))) %>%
        filter(! (type=="quantile" & is.na(quantile))) %>%
        left_join( obs_data %>% rename(report = value_inc),
                   by=c("target_end_date", "location_name", "location", "age_group"))%>%

        rename(date = target_end_date) %>%
        filter(!is.na(report))

    dat_for_scores = distinct(dat_for_scores)
    return(dat_for_scores)
}
