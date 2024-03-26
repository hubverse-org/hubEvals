
#' Title
#'
#' @param wis_data
#'
#' @return
#' @export
#'
#' @examples


abswis_fig <- function(wis_data) {

  require(magrittr)

  abs_states <- wis_data %>%
    dplyr::filter(location_name != "US") %>%
    dplyr::group_by(model, target_end_date, target, horizon) %>%
    dplyr::summarise(abs_WIS = mean(WIS)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(log_WIS = log10(abs_WIS))

  # abs_ens <- abs_states %>% filter(model %in% contains("ensemble"))
  #
  # '%!in%' <- Negate('%in%')
  # abs_not_ens <- abs_states %>% filter(model %!in% contains("ensemble"))
  #
  wis_labels <- as_labeller(c(`0` = "0 Weeks Ahead",
                              `1` = "1 Week Ahead",
                              `2` = "2 Weeks Ahead",
                              `3` = "3 Weeks Ahead",
                              `4` = "4 Weeks Ahead"))

  fig_absWIS <- ggplot2::ggplot(abs_states, ggplot2::aes(x = target_end_date, y = abs_WIS, group = model, col = model)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(y = "Absolute WIS",
         x = "Forecast Target End Date",
         color = "Model",
         title = "Absolute WIS by Model") +
    ggplot2::theme_bw() +
    ggplot2::scale_x_date(breaks = seq.Date(from = min(abs_states$target_end_date), to= max(abs_states$target_end_date), by = "1 week"), date_labels = "%d %b") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1), panel.grid = ggplot2::element_blank()) +
    ggplot2::facet_grid(rows = vars(horizon), labeller = wis_labels,  scales = "free_x")

  return(fig_absWIS)
}
