#' Resident needs plot
#'
#' Generates the plot breaking down the resident needs
#'
#' @param d Your dataset geocoded to neighborhood
#'
#' @return A .png image of the plot
#' @export
#'
#' @examples
#' needs_plot(d)
needs_plot <- function(d) {

  d_needs <- d |>
    select(record_id, redcap_event_name, neighborhood, contains('needs')) |>
    select(-contains('contact'))

  d_needs <- d_needs |>
    pivot_longer(cols = contains('needs'), names_to = "q", values_to = "need") |>
    group_by(record_id) |>
    distinct(need, .keep_all = T) |>
    drop_na(need)

  d_needs <- d_needs |>
    mutate(need = dplyr::recode(need,
                                "Comida" = "Food",
                                "Vivienda" = "Housing",
                                "Servicios públicos" = "Utilities",
                                "Finanzas" = "Finances",
                                "Transporte" = "Transportation",
                                "Soledad o aislamiento social" = "Loneliness or social isolation",
                                "Seguridad personal" = "Personal safety",
                                "Empleo" = "Employment",
                                "Cuidadados infantiles" = "Childcare",
                                "Cuidados infantiles" = "Childcare",
                                "Otro" = "Other",
                                "No necesito ayuda con ninguno de estos" = "I don't need help with any of these"))

  d_needs_sum <- d_needs |>
    group_by(neighborhood, need) |>
    summarise(num_need = n()) |>
    filter(num_need > 1) |>
    arrange(desc(num_need))

  needs_plot <- ggplot(d_needs_sum, aes(x = reorder(need, num_need), y = num_need)) +
    geom_col(fill = "lightsalmon1") +
    coord_flip() +
    theme_light() +
    labs(y = "", title = "Resident Needs", x = "") +
    facet_wrap('neighborhood')

  ggsave(paste0("./Plots_Figures/needs_plot_",Sys.Date(),".png"), plot = needs_plot, width = 9, height = 7)
}
