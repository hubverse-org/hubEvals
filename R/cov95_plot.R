#' Title
#'
#' @param WIS_data
#'
#' @return
#' @export
#'
#' @examples

cov95_plot <- function(WIS_data){

    coverage95_states <- WIS_data %>% filter(location_name != "US") %>%
        filter(horizon == 0| horizon == 1) %>%
        group_by(model, target_end_date, target) %>%
        summarise(coverage95 = mean(coverage.95)) %>%
        #mutate(model_color = ifelse(model == "hub-ensemble", "#6baed6", "#abbfcb")) %>%
        ungroup()

    # coverage95_ens <- coverage95_states %>% filter(model %in% c("hub-ensemble"))
    # coverage95_not_ens <- coverage95_states %>% filter(model %!in% c("hub-ensemble"))
    #
    coverage_labels <- as_labeller(c(`0` = "0 Weeks Ahead",
                                     `1` = "1 Week Ahead"))

    figure <- ggplot(coverage95_states, aes(x = target_end_date,
                                           y = coverage95, group = model,
                                           col = model)) +
        geom_line(linewidth = 1) + geom_point(size = 2) +
      #  geom_line(data = coverage95_not_ens, aes(x = target_end_date, y = coverage95, group = model), color = adjustcolor("grey50", .35)) +
        labs(y = "95% Coverage",
             x = "",
             color = "Model",
             title = "95% Coverage by Model") +
      #  scale_color_manual(values = c("#d6936b", "#6baed6")) +
        theme_bw()+
        scale_x_date(breaks = seq.Date(from = min(coverage95_states$target_end_date), to= max(coverage95_states$target_end_date), by = "1 week"), date_labels = "%d %b") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1), panel.grid = element_blank())
    #facet_grid(rows = vars(target), labeller = coverage_labels, scales = "free_x")

    return(figure)

}
