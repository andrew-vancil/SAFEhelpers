#' Likert (Agree-Disagree) Plot
#'
#' Generates a likert plot of the agree-disagree spectrum questions
#'
#' @param d Your dataset geocoded to neighborhood. Needs the columns to retain their original order
#'
#' @return A .png image of the plot
#' @export
#'
#' @examples
#' likert_plot(d)
likert_plot <- function(d) {

  d_likert_all <- d[,c(127:133, 184:190, 203)]

  d_likert_all <- d_likert_all |>
    mutate(enough_stores = coalesce(enough_food_stores_eng, food_community_span),
           get_what_want = coalesce(can_get_what_i_want_eng, food_want_span),
           healthy_avail = coalesce(healthy_available_eng, healthy_options_span),
           healthy_afford = coalesce(healthy_affordable_eng, healthy_affordable_span),
           comm_hungry = coalesce(hungry_in_community_eng, community_hungry_span),
           eat_healthy_if_avail = coalesce(eat_healthy_if_avail_eng, eat_healthy_available_span),
           eat_healthy_if_cook = coalesce(healthy_if_cook_eng, cook_healthy_span)) |>
    select(-contains('eng'), -contains('span')) |>
    drop_na()

  d_likert_all <- d_likert_all |>
    mutate(across(.cols = everything(), ~dplyr::recode(., "Totalmente en desacuerdo" = "Strongly Disagree",
                                                       "En desacuerdo" = "Disagree",
                                                       "No está seguro/a" = "Unsure",
                                                       "De acuerdo" = "Agree",
                                                       "Totalmente de acuerdo" = "Strongly Agree")))

  d_likert_all <- as.data.frame(d_likert_all)
  d_likert_all$enough_stores <- factor(d_likert_all$enough_stores, levels = c('Strongly Disagree', 'Disagree', 'Unsure', 'Agree', 'Strongly Agree'))
  d_likert_all$get_what_want <- factor(d_likert_all$get_what_want, levels = c('Strongly Disagree', 'Disagree', 'Unsure', 'Agree', 'Strongly Agree'))
  d_likert_all$healthy_avail <- factor(d_likert_all$healthy_avail, levels = c('Strongly Disagree', 'Disagree', 'Unsure', 'Agree', 'Strongly Agree'))
  d_likert_all$healthy_afford <- factor(d_likert_all$healthy_afford, levels = c('Strongly Disagree', 'Disagree', 'Unsure', 'Agree', 'Strongly Agree'))
  d_likert_all$comm_hungry <- factor(d_likert_all$comm_hungry, levels = c('Strongly Disagree', 'Disagree', 'Unsure', 'Agree', 'Strongly Agree'))
  d_likert_all$eat_healthy_if_avail <- factor(d_likert_all$eat_healthy_if_avail, levels = c('Strongly Disagree', 'Disagree', 'Unsure', 'Agree', 'Strongly Agree'))
  d_likert_all$eat_healthy_if_cook <- factor(d_likert_all$eat_healthy_if_cook, levels = c('Strongly Disagree', 'Disagree', 'Unsure', 'Agree', 'Strongly Agree'))
  d_likert_all$neighborhood <- factor(d_likert_all$neighborhood, levels = c('Avondale', 'W. Price Hill', 'E. Price Hill', 'Lower Price Hill-Queensgate'))

  d_likert_all <- d_likert_all |>
    drop_na(neighborhood)

  names(d_likert_all) <- c(
    neighborhood = "neighborhood",
    enough_stores = "There are enough food stores",
    get_what_want= "I can get the types of food I want",
    healthy_avail = "Healthy choices are available",
    healthy_afford = "Healthy food choices are affordable",
    comm_hungry = "People in my community are going hungry",
    eat_healthy_if_avail= "We would eat healthier food if the local stores offered more healthy options",
    eat_healthy_if_cook = "We would eat healthier food if I knew how to cook them"
  )

  d_likert_all <- as.data.frame(d_likert_all)

  all_likert <- plot(likert(d_likert_all[,c(2:8)], grouping = d_likert_all$neighborhood), wrap = 25,
                     group.order = c("Avondale", "W. Price Hill", "E. Price Hill", "Lower Price Hill-Queensgate"))


  ggsave(paste0("./Plots_Figures/agree_disagree_plot_",Sys.Date(),".png"), plot = all_likert, width = 8, height = 12)


}
