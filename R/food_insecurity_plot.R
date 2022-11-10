#' Food insecurity plot
#'
#' Generates the plot of food insecurity over time
#'
#' @param d Your dataset geocoded to neighborhood
#'
#' @return A .png image of the plot
#' @export
#'
#' @examples
#' food_insecurity_plot(d)
food_insecurity_plot <- function(d) {

  d_insec <- d |>
    select(record_id, redcap_event_name, food_equity_english_timestamp, food_equity_spanish_timestamp, contains('worr'), contains('run'), neighborhood) |>
    mutate(worry_span = dplyr::recode(worry_span, "Verdadero con frecuencia" = "Often true",
                                      "Verdadero a veces" = "Sometimes true",
                                      "Nunca fue verdadero" = "Never true"),
           runout_span = dplyr::recode(worry_span, "Verdadero con frecuencia" = "Often true",
                                       "Verdadero a veces" = "Sometimes true",
                                       "Nunca fue verdadero" = "Never true"))

  d_insec <- d_insec |>
    mutate(worry = coalesce(worried_eng, worry_span),
           run_out = coalesce(run_out_eng, runout_span),
           timestamp = coalesce(food_equity_english_timestamp, food_equity_spanish_timestamp)) |>
    drop_na(worry) |>
    select(-contains("span"), -contains("eng"))

  d_insec <- d_insec |>
    mutate(insecurity = ifelse(worry == "Never true" & run_out == "Never true", "secure", "insecure")) |>
    mutate(severity = ifelse(worry == "Never true" & run_out == "Never true", 1,
                             ifelse(worry == 'Often true' |  run_out == 'Often true', 3, 2)))

  d_insec <- d_insec |>
    mutate(week = week(timestamp))

  d_insec$week_date <- as.Date(paste(2022, d_insec$week, 1, sep = "-"), "%Y-%U-%u")


  d_insec_sev <- d_insec |>
    group_by(neighborhood, week_date, severity) |>
    summarise(cnt = n(), .groups = "drop_last") |>
    mutate(pct_severity = round(cnt / sum(cnt) * 100, 2)) |>
    ungroup() |>
    drop_na(severity)

  d_insec_2 <- d_insec |>
    group_by(neighborhood, week_date, insecurity) |>
    summarise(cnt_insec = n(), .groups = "drop_last") |>
    mutate(pct_insecure = round(cnt_insec / sum(cnt_insec) * 100, 2)) |>
    filter(insecurity == "insecure")


  d_insec_sev$severity <- factor(d_insec_sev$severity, labels = c('1' = "Neither worried \n nor ran out of food",
                                                                  '2' = "Sometimes worried \n or ran out of food",
                                                                  '3' = "Often worried \n or ran out of food"))

  insecurity_over_time <- ggplot() +
    geom_area(data = d_insec_sev, aes(x = week_date, y = pct_severity, group = severity,
                                      fill = severity), col = "white", size = .6, alpha = 1) +
    geom_line(data = d_insec_2, aes(x = week_date, y = pct_insecure, col = insecurity), size = 2) +
    scale_fill_manual(values = c("mediumseagreen", "lightgoldenrod1", "goldenrod1")) +
    scale_color_manual(values = c("firebrick1"), labels = str_wrap("Total Percent Food Insecure", 13)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,101)) +
    CB::theme_cb() +
    theme(legend.key.height = unit(1.5, "cm")) +
    labs(x = "Date", y = "Percentage", fill = "Food Insecurity", col = "") +
    ggeasy::easy_rotate_x_labels(angle = 90) +
    facet_wrap("neighborhood")


  ggsave(paste0("./Plots_Figures/insecurity_plot_",Sys.Date(),".png"), plot = insecurity_over_time, width = 9, height = 7)
}
