#' Title
#'
#' @param wis_data
#'
#' @return
#' @export
#'
#' @examples


wis_density_plot <- function(wis_data){

  require(dplyr)
  require(ggplot2)
  require(ggridges)

  inc_scores_overall <- WIS_data %>%
    # filter(include_overall == "TRUE") %>%
    group_by(target_end_date, target, location_name) %>%
    mutate(n_models = n()) %>%
    arrange(WIS) %>%
    mutate(model_rank = row_number(), rank_percentile = model_rank/n_models) %>%
    arrange(-WIS) %>%
    mutate(rev_rank = (row_number()-1)/(n_models-1)) %>%
    ungroup() %>%
    mutate(model = reorder(model, rev_rank, FUN=function(x) quantile(x, probs=0.25, na.rm=TRUE)))

  n_unique_predict<- inc_scores_overall %>%
    group_by(target_end_date, location_name, target) %>%
    summarize(n()) %>%
    nrow()

  average_rank_percent <- inc_scores_overall %>%
    group_by(model) %>%
    summarize(average_rank = mean(rev_rank), total_n = n(),
              n_top50 = sum(rev_rank> 0.5) , pct_top50 = n_top50/total_n*100,
              n_top25 = sum(rev_rank> 0.75) , pct_top25 = n_top25/total_n*100,
              n_bottom50 = sum(rev_rank< 0.5) , pct_bottom50 = n_bottom50/total_n*100,
              n_bottom25 = sum(rev_rank< 0.25) , pct_bottom25 = n_bottom25/total_n*100) %>%
    #print(n=Inf) %>%
    arrange(-pct_top50)

  figure2 <- inc_scores_overall %>%
    ggplot(aes(y = model, x=rev_rank, fill = factor(after_stat(quantile)))) +
    stat_density_ridges(
      geom = "density_ridges_gradient", calc_ecdf = TRUE,
      quantiles = 4, quantile_lines = TRUE, color = "gray30"
    ) +
    scale_fill_manual(values = c("#6baed6", "#c86bd6","#d6936b","#78d66b"), name = "Quartiles")+
    labs(x = "Standardized Rank", y = "Model", color = "Quartiles")+
    scale_x_continuous(limits=c(0,1)) +
    theme_bw() +
    theme(strip.text = element_text(size = 10))

  return(figure2)

}
