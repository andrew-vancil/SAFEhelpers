#' Respondent summary by zip code
#'
#' Calculate the number of completed responses by zip code
#'
#' @param d Your dataset geocoded to neighborhood
#'
#' @return A dataframe with the number of responses by zip code
#' @export
#'
#' @examples
#' d_zip <- zip_totals(d)
zip_totals <- function(d) {
  d_num_zip <- d |>
    select(record_id, demo_zip_eng, demo_zip_span) |>
    mutate(zip = coalesce(demo_zip_eng, as.character(demo_zip_span))) |>
    select(record_id, zip) |>
    drop_na(zip) |>
    group_by(zip) |>
    summarise(num_zip = n())

  return(d_num_zip)
}
