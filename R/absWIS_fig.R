
#' Title
#'
#' @param WIS_data
#'
#' @return
#' @export
#'
#' @examples


absWIS_fig <- function(WIS_data){

    abs_states <- WIS_data %>% filter(location_name != "US") %>%
        group_by(model, target_end_date, target) %>%
        summarise(abs_WIS = mean(WIS)) %>%
        ungroup() %>% mutate(log_WIS = log10(abs_WIS))

    # abs_ens <- abs_states %>% filter(model %in% contains("ensemble"))
    #
    # '%!in%' <- Negate('%in%')
    # abs_not_ens <- abs_states %>% filter(model %!in% contains("ensemble"))
    #
    # wis_labels <- as_labeller(c(`1 wk ahead inc flu hosp` = "1 Week Ahead",
    #                                  `4 wk ahead inc flu hosp` = "4 Week Ahead"
    #                             ))

    #Absolute WIs version - not log scores
    fig_absWIS <- ggplot(abs_states, aes(x = target_end_date,
                                      y = abs_WIS, group = model,
                                      col = model)) +
        geom_line(size = 1) + geom_point(size = 2) +
       # scale_color_manual(values = c("#d6936b", "#6baed6")) +
      #  geom_line(data = abs_not_ens, aes(x = target_end_date, y = abs_WIS, group = model), color = adjustcolor("grey50", .35)) +
        labs(y = "Absolute WIS",
             x = "Forecast Target End Date",
             color = "Model",
             title = "Absolute WIS by Model") +
        theme_bw()+
        scale_x_date(breaks = seq.Date(from = min(abs_states$target_end_date), to= max(abs_states$target_end_date), by = "1 week"), date_labels = "%d %b") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1), panel.grid = element_blank())
    #   facet_grid(rows = vars(target), cols = vars(season), labeller = wis_labels,  scales = "free_x")

    return(fig_absWIS)
}
