#' Function that outputs a data frame with all of the WIS scores
#'
#' @param obs_data Ground truth data file
#' @param weeks.to.eval Sequence of dates to be evaluated
#' @param output_path File path to directory where output data are stored
#' @param target1 Target of interest; for example, 'inc hosp'
#' @param loc_data Data file containing location information, including FIPS codes and location names
#'
#'
#' @return
#' @export
#'
#' @examples

get_results_data <- function(obs_data, weeks.to.eval, output_path, target1, loc_data){

    require(dplyr)

    proj_data <- get_data_tibble(output_path, loc_data, weeks.to.eval)

    obs_data <- format_obs_data(obs_data, proj_data, target1)

    dat_for_scores <- dat_for_scores_func(proj_data, obs_data)

    score_df <- dat_for_scores %>%
        mutate(alpha=ifelse(quantile < 0.500, quantile*2, 2*(1-quantile)), # Sets alpha levels for each interval
               indicator = ifelse(quantile < 0.500 & report < value | quantile > 0.500 & report > value, 1, 0), #low indicator & high indicator
               pen = ifelse(indicator==1 & quantile < 0.500, (2/alpha)*(value-report), #low penalty
                            ifelse(indicator==1 & quantile > 0.500, (2/alpha)*(report-value), 0)) #high penalty
        )

    IS_per_alpha <- score_df %>%
        mutate(alpha=as.factor(alpha)) %>%
        group_by(model, date, location_name, age_group, forecast_date, alpha) %>%
        summarize(value_diff=max(value)-min(value), #difference between forecasted value at max quantile and min quantile
                  pen=sum(pen), .groups = 'keep') %>% #sum of penalty for both quantiles within intervals
        ungroup() %>%
        mutate(alpha=as.numeric(as.character(alpha)),
               IS=(alpha/2)*(value_diff+pen)) %>% # Create interval scores
        filter(alpha!="1.00")

    IS_sum <- IS_per_alpha %>%
        group_by(model, date, location_name, age_group, forecast_date) %>%
        summarize(IS_sum=sum(IS), .groups = 'keep') #Sum IS across all available intervals

    num_interval<-length(unique(IS_per_alpha$alpha))-1

    Q50 <- score_df %>%
        filter(quantile==0.500) %>%
        mutate(IS_Q50=(alpha/2)*abs(report-value))

    WIS_calc <- IS_sum %>%
        left_join(., Q50, by=c("model", "date","age_group", "location_name", "forecast_date")) %>%
        mutate(WIS = (1/ (0.5 + num_interval))*(IS_Q50 + IS_sum),
               forecast_date=as.Date(forecast_date)) %>%
        dplyr::select(model,date, location_name, age_group, forecast_date, WIS) %>%
        as.data.frame()

    score_df2 <- dat_for_scores %>%
        filter(quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975)) %>%
        filter(type == "quantile") %>%
        dplyr::select(-contains('value_inc'), -contains("output_type_id"),-contains("loc_abbr"), -contains("age_id"), -contains("location_id")) %>%
        pivot_wider(names_from = c(type, quantile), values_from=value)

    WIS_all <- left_join(score_df2, WIS_calc) %>%
        mutate(WIS_rel = ifelse(report==0, WIS/1, WIS/report),
               abs.error = abs(quantile_0.5 - report),
               perc.point.error = 100*abs.error/report,
               coverage.50 = ifelse(report >= quantile_0.25 & report <= quantile_0.75,T,F),
               coverage.95 = ifelse(report >= quantile_0.025 & report <= quantile_0.975,T,F)
        ) %>%
        rename(target_end_date = date)

    WIS <- filter(WIS_all, as.Date(forecast_date) >= as.Date(min(weeks.to.eval)), as.Date(forecast_date) <= as.Date(max(weeks.to.eval))) %>% {unique(.)}

    return(WIS)
}
