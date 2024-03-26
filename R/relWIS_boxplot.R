#' Title
#'
#' @param wis_data
#'
#' @return
#' @export
#'
#' @examples


relwis_boxplot <- function(wis_data){

  require(dplyr)
  require(ggplot2)
  require(forcats)

  inc.rankings_all <- inc_rankings_func(WIS_data)
  inc.rankings_all_nice <- inc.rankings_all %>% group_by(model) %>% arrange(mean.WIS) %>% mutate(modelorder = paste(model))

  inc.rankings_location <- WIS_ranking_location(WIS_data)
  inc.rankings_location$below <- ifelse(inc.rankings_location$relative_WIS < 1, 1, 0)

  model_order <- merge(inc.rankings_all_nice[,1:3], inc.rankings_location, by = "model", all.y = TRUE) %>% arrange(rel.WIS.skill)

  figs1 <- model_order %>%
    ggplot( aes(x = fct_inorder(model), y = relative_WIS))+
    geom_boxplot()+
    theme_bw() +
    geom_hline(aes(yintercept = 1), color = "#6baed6")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.margin = margin(0,1,0,25))+
    labs(x = "Model", y = "Relative WIS")

  return(figs1)
}
