#' Barriers Plot
#'
#' Generate the plot of resident barriers to healthy food
#'
#' @param d Your dataset geocoded to neighborhood
#'
#' @return A .png image of the plot
#' @export
#'
#' @examples
#' barriers_plot(d)
barriers_plot <- function(d) {

  d_bar <- d |>
    select(record_id, redcap_event_name, neighborhood, contains('barrier'))

  d_bar <- d_bar |>
    pivot_longer(cols = contains('bar'), names_to = "q", values_to = "bar") |>
    group_by(record_id) |>
    distinct(bar, .keep_all = T) |>
    drop_na(bar)

  d_bar <- d_bar |>
    mutate(bar = dplyr::recode(bar,
                               "Costo de la comida saludable" = "Cost of healthy foods",
                               "Acceso a la comida saludable" = "Access to healthy foods",
                               "Almacenamiento limitado para la comida saludable en casa" = "Limited storage for healthy foods at home",
                               "Conocimiento sobre la comida saludable" = "Knowledge about healthy foods",
                               "Equipamiento para preparar comida saludable" = "Equipment to cook healthy foods",
                               "Tiempo" = "Time",
                               "Otro" = "Other"))

  d_bar_sum <- d_bar |>
    group_by(neighborhood, bar) |>
    summarise(num_bar = n()) |>
    filter(num_bar > 1) |>
    arrange(desc(num_bar))

  barriers_plot <- ggplot(d_bar_sum, aes(x = reorder(bar, num_bar), y = num_bar)) +
    geom_col(fill = "lightsalmon1") +
    coord_flip() +
    theme_light() +
    labs(y = "", title = "Barriers to Healthy Foods", x = "") +
    facet_wrap('neighborhood')

  ggsave(paste0("./Plots_Figures/barriers_plot_",Sys.Date(),".png"), plot = barriers_plot, width = 9, height = 7)
}
