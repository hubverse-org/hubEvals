
#' Transform observed data into appropriate format *NOT FINISHED*
#'
#' @param obs_data
#' @param proj_data
#' @param target1
#'
#' @return
#' @export
#'
#' @examples
#'

# NOT COMPLETE

format_obs_data <- function(obs_data, proj_data, target1){

    require(dplyr)

    all_dat <- proj_data

    obs_data_all <- gtruth_data %>%
        mutate(wk_end_date = as.Date(date, "%y/%m/%d"),
               location_name = as.character(location_id)) %>%
        filter(wk_end_date %in% as.Date(unique(all_dat$target_end_date)),
               target == target1)

    obs_data <- obs_data_all %>%
        rename(value_inc = value,
               target_end_date = wk_end_date,
               date1 = date) %>%
        dplyr::select(-target) %>%
        filter(target_end_date < Sys.Date())

}
