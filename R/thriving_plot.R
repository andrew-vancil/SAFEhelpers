#' Thriving plot
#'
#' Generates the plot of thriving over time
#'
#' @param d Your dataset geocoded to neighborhood
#'
#' @return A .png image of the plot
#' @export
#'
#' @examples
#' thriving_plot(d)
#'
thriving_plot <- function(d) {

  d_thrive <- d |>
    select(record_id, redcap_event_name, food_equity_english_timestamp, food_equity_spanish_timestamp,
           contains('ladder'), neighborhood) |>
    mutate(ladder_current_eng = round(ladder_current_eng/10, 0),
           ladder_5yrs_eng = round(ladder_5yrs_eng/10, 0)) |>
    mutate(ladder_current = coalesce(ladder_current_eng, ladder_current_span),
           ladder_5yrs = coalesce(ladder_5yrs_eng, ladder_5yr_span),
           timestamp = coalesce(food_equity_english_timestamp, food_equity_spanish_timestamp)) |>
    mutate(thrive = ifelse(ladder_current >= 7 & ladder_5yrs >=8, "Thriving",
                           ifelse(ladder_current <= 4 & ladder_5yrs <=4, "Suffering", "Struggling"))) |>
    select(-contains("eng"), -contains("span"))

  d_thrive <- d_thrive |>
    mutate(week = week(timestamp))

  d_thrive$week_date <- as.Date(paste(2022, d_thrive$week, 1, sep = "-"), "%Y-%U-%u")

  d_thrive <- d_thrive |>
    drop_na(thrive) |>
    group_by(neighborhood, week_date, thrive) |>
    summarise(cnt = n(),
              .groups = NULL) |>
    mutate(pct_thrive = round(cnt / sum(cnt) * 100, 2))

  d_thrive$thrive <- factor(d_thrive$thrive, levels = c("Thriving", "Struggling", "Suffering"))

  thrive_over_time <- ggplot(d_thrive, aes(x = week_date, y = pct_thrive, group = thrive, fill = thrive)) +
    geom_area(col = "white", size = .6, alpha = 1) +
    scale_fill_manual(values = c("mediumseagreen", "lightgoldenrod1", "indianred")) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,101)) +
    CB::theme_cb() +
    labs(x = "Date", y = "Percentage", fill = "Thriving Status") +
    ggeasy::easy_rotate_x_labels(angle = 90) +
    facet_wrap('neighborhood')

  ggsave(paste0("./Plots_Figures/thriving_plot_", Sys.Date(),".png"),
         plot = thrive_over_time, width = 9, height = 7)

}
