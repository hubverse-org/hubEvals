#' Title
#'
#' @param wis_data
#'
#' @return
#' @export
#'
#' @examples

abswis_table <- function(wis_data) {

  require(magrittr)

  inc.rankings_all <- inc_rankings_func(wis_data)
  inc.rankings_all_nice <- inc.rankings_all %>%
    dplyr::group_by(model) %>%
    dplyr::arrange(mean.WIS) %>%
    dplyr::mutate(modelorder = paste(model))

  inc.rankings_location <- wis_ranking_location(wis_data)
  inc.rankings_location$below <- ifelse(inc.rankings_location$relative_WIS < 1, 1, 0)

  scores <- inc.rankings_location %>%
    dplyr::filter(is.finite(mean.WIS)) %>%
    dplyr::left_join(., y = inc.rankings_all_nice[,c("model","modelorder")], by = c("model"))
  scores_order <- inc.rankings_all_nice
  levels_order <- scores_order$modelorder

  figure3 <- ggplot2::ggplot(scores, aes(x = factor(modelorder, levels = levels_order),
                                         y=location_name,
                                         fill= scales::oob_squish(mean.WIS, range = c(0,200)))) +
    ggplot2::geom_tile() +
    ggplot2::theme_bw() +
    ggplot2::geom_text(ggplot2::aes(label = signif(mean.WIS, 2)), size = 2.5) +
    ggplot2::scale_fill_gradient2(low ="#6baed6", high =  "#d6936b", midpoint = 1, na.value = "grey50", name = "Mean WIS") +
    #breaks = c(-2,-1,0,1,2),
    #labels =c(0.25, 0.5, 1, 2, 4)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
          axis.title.x = ggplot2::element_text(size = 9),
          axis.text.y = ggplot2::element_text(size = 7),
          title = ggplot2::element_text(size = 9)) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::scale_x_discrete(labels = function(x) substring(x, 1, nchar(x)-10)) +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank())

  return(figure3)
}
