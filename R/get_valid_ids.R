#' Filter to valid responses
#'
#' Filters your redcap dataset to only valid record IDs. Also eliminates rows with a duration less than 15 seconds and where respondents completed both language surveys
#'
#' @param d Your raw redcap dataset
#'
#' @return Your validated dataset
#' @export
#'
#' @examples
#' d <- get_valid_ids(d)
#'
get_valid_ids <- function(d) {

  valid_ids <- d |>
    filter(valid == "valid" | valid_span == "valid") |>
    pull(record_id)

  out <- d |>
    filter(record_id %in% valid_ids) |>
    rowwise() |>
    mutate(duration = food_equity_english_timestamp - start_timestamp_eng,
           duration_s = food_equity_spanish_timestamp - start_timestamp_span) |>
    filter(is.na(duration) | is.na(duration_s) | duration > 15 | duration_s > 15)

  return(out)
}


