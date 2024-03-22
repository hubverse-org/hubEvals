#' Title
#'
#' @param WIS_data
#'
#' @return
#' @export
#'
#' @examples

absWIS_table <- function(WIS_data){

    require(dplyr)
    require(ggplot2)

    inc.rankings_all <- inc_rankings_func(WIS_data)
    inc.rankings_all_nice <- inc.rankings_all %>% group_by(model) %>% arrange(mean.WIS) %>% mutate(modelorder = paste(model))

    inc.rankings_location <- WIS_ranking_location(WIS_data)
    inc.rankings_location$below <- ifelse(inc.rankings_location$relative_WIS < 1, 1, 0)

    scores <- inc.rankings_location %>% filter(is.finite(mean.WIS)) %>% left_join(., y = inc.rankings_all_nice[,c("model","modelorder")], by = c("model"))
    scores_order <- inc.rankings_all_nice
    levels_order <- scores_order$modelorder

    figure3 <- ggplot(scores,
                      aes(x = factor(modelorder, levels = levels_order), y=location_name,
                          fill= scales::oob_squish(mean.WIS, range = c(0,200)))) +
        geom_tile() +
        theme_bw()+
        geom_text(aes(label = signif(mean.WIS, 2)), size = 2.5) +
        scale_fill_gradient2(low ="#6baed6", high =  "#d6936b", midpoint = 1, na.value = "grey50",
                             name = "Mean WIS")+
        #breaks = c(-2,-1,0,1,2),
        #labels =c(0.25, 0.5, 1, 2, 4)) +
        labs(x = NULL, y = NULL)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
              axis.title.x = element_text(size = 9),
              axis.text.y = element_text(size = 7),
              title = element_text(size = 9)
        ) +
        scale_y_discrete(limits = rev) +
        scale_x_discrete(labels = function(x) substring(x, 1, nchar(x)-10))+
        theme(axis.ticks.y = element_blank())

     return(figure3)

}
