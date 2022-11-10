#' Basic SAFE demographics
#'
#' Calculate basic demographics information from the SAFE survey responses. Outputs a list with three datasets: the number of surveys completed per person, the number of kids for each household, and a table of summaries
#'
#' @param d Your dataset geocoded to neighborhood
#'
#' @return A list with three datasets
#' @export
#'
#' @examples
#' demos_list <- basic_demos(d)
basic_demos <- function(d) {

  num_participants <- as.double(nrow(distinct(d, record_id)))

  d_num_surv <- d |>
    group_by(record_id) |>
    filter(food_equity_english_complete == 'Complete' | food_equity_spanish_complete == 'Complete') |>
    summarise(num_surv_complete = n())

  avg_num_surv <- round(mean(d_num_surv$num_surv_complete), 1)
  total_num_surv <- as.double(sum(d_num_surv$num_surv_complete))

  d_num_kids <- d |>
    select(record_id, demo_num_kids_eng, demo_num_kids_span) |>
    mutate(num_kids = coalesce(demo_num_kids_eng, demo_num_kids_span)) |>
    select(record_id, num_kids) |>
    distinct(record_id, .keep_all = T)

  avg_kids <- round(mean(d_num_kids$num_kids), 1)

  out_1 <- tibble(num_participants,
                avg_num_surv,
                total_num_surv,
                avg_kids)
  out <- list(d_num_surv, d_num_kids, out_1)

  return(out)
}
