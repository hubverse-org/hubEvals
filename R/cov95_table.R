
#' Creates a 95% prediction interval coverage table
#'
#' @param WIS_data
#'
#' @return
#' @export
#'
#' @examples

cov95_table <- function(WIS_data){

    require(knitr)

    weekly_breakdown <- WIS_data %>% group_by(model) %>% reframe(
        model = model,
        Zero_week_Cov = mean(coverage.95[horizon == 0], na.rm = TRUE)*100,
        One_week_Cov = mean(coverage.95[horizon == 1], na.rm = TRUE)*100,
        Two_week_Cov = mean(coverage.95[horizon == 2], na.rm = TRUE)*100,
        Three_week_Cov = mean(coverage.95[horizon == 3], na.rm = TRUE)*100,
        Four_week_Cov = mean(coverage.95[horizon == 4], na.rm = TRUE)*100
    ) %>% unique()

    table <- knitr::kable(weekly_breakdown, align = c("lcccccccccc"), caption = "95% Coverage Table", col.names = c("Model","Week 0 Coverage", "1 Wk Coverage", "2 Wk Coverage", "3 Wk Coverage", "4 Wk Coverage")) %>%
        kableExtra::kable_classic()  #%>%

    return(table)
}
