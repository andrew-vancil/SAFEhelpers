#' Respondent summary by race
#'
#' @param d Your dataset geocoded to neighborhood
#'
#' @return A dataframe with the number of responses by zip code
#' @export
#'
#' @examples
#' d_race <- race_totals(d)
race_totals <- function(d) {
  d_race <- d |>
    select(record_id, contains('demo_race')) |>
    mutate(race = coalesce(demo_race_eng___1, demo_race_eng___2, demo_race_eng___3, demo_race_eng___4, demo_race_eng___5,
                           demo_race_eng___6, demo_race_eng___7, demo_race_eng___8, demo_race_eng___9, demo_race_eng___10,
                           demo_race_span)) |>
    select(record_id, race) |>
    drop_na(race) |>
    group_by(race) |>
    summarise(num_race = n())

  return(d_race)
}
