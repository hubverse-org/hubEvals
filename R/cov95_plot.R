#' Plot of 95% coverage by forecast horizon
#'
#' @param wis_data
#'
#' @return
#' @export
#'
#' @examples

cov95_plot <- function(wis_data){

  require(magrittr)

  coverage95_states <- wis_data %>%
    dplyr::filter(location_name != "US") %>%
    dplyr::group_by(model, target_end_date, target, horizon) %>%
    dplyr::summarise(coverage95 = mean(coverage.95)) %>%
    dplyr::ungroup()

  coverage_labels <- as_labeller(c(`0` = "0 Weeks Ahead",
                              `1` = "1 Week Ahead",
                              `2` = "2 Weeks Ahead",
                              `3` = "3 Weeks Ahead",
                              `4` = "4 Weeks Ahead"))

  figure <- ggplot2::ggplot(coverage95_states, ggplot2::aes(x = target_end_date, y = coverage95, group = model, col = model)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(y = "95% Coverage",
         x = "",
         color = "Model",
         title = "95% Coverage by Model") +
    ggplot2::theme_bw() +
    ggplot2::scale_x_date(breaks = seq.Date(from = min(coverage95_states$target_end_date), to= max(coverage95_states$target_end_date), by = "1 week"), date_labels = "%d %b") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1), panel.grid = ggplot2::element_blank()) +
    ggplot2::facet_grid(rows = vars(horizon), labeller = coverage_labels, scales = "free_x")

  return(figure)

}
