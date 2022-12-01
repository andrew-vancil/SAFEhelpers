#' Monthly Gift Card List
#'
#' Input your REDCap project url and token and generate a csv file containing all eligible gift card recipients for the past month
#'
#' @param url Your redcap project url
#' @param token Your personal redcap project token
#'
#' @return
#' @export
#'
#' @examples
#' gift_card_list(my_url, my_token)
#'
library(dplyr)
gift_card_list <- function(url, token) {
  my_url <- url
  my_token <- token

  data <- REDCapR::redcap_read(redcap_uri = my_url,
                      token = my_token,
                      export_checkbox_label = TRUE,
                      export_survey_fields = TRUE,
                      raw_or_label = 'label')$data

  dates <- seq.Date(((Sys.Date()+1) - 31), Sys.Date(), by = 1)

  d_gc <- data |>
    select(record_id, food_equity_english_complete, food_equity_spanish_complete, food_equity_english_timestamp, food_equity_spanish_timestamp,
           demographics_english_complete, demographics_spanish_complete, demographics_english_timestamp, demographics_spanish_timestamp,
           start_timestamp_eng, start_timestamp_span)

  d_gc <- d_gc |>
    filter(food_equity_english_complete == 'Complete' | food_equity_spanish_complete == 'Complete' | demographics_english_complete == 'Complete' | demographics_spanish_complete == 'Complete') |>
    filter(lubridate::date(food_equity_english_timestamp) %in% dates | lubridate::date(food_equity_spanish_timestamp) %in% dates |
             lubridate::date(demographics_english_timestamp) %in% dates | lubridate::date(demographics_spanish_timestamp) %in% dates)

  #only keep weeks where people took one survey and took longer than 15 seconds
  d_gc <- d_gc |>
    rowwise() |>
    filter(is.na(food_equity_english_timestamp) | is.na(food_equity_spanish_timestamp)) |>
    mutate(duration = food_equity_english_timestamp - start_timestamp_eng,
           duration_s = food_equity_spanish_timestamp - start_timestamp_span) |>
    filter(duration > 15 | duration_s > 15)


  d_gc <- d_gc |>
    group_by(record_id) |>
    mutate(num_complete = n()) |>
    select(record_id, num_complete) |>
    distinct(record_id, .keep_all = T) |>
    filter(num_complete > 1)

  d_contact <- data |>
    select(record_id, consent_name_eng, consent_name_span, consent_email, consent_phone,
           consent_how_gift_card_eng, consent_how_gift_card_span) |>
    group_by(record_id) |>
    slice(1)

  d_gc_final <- left_join(d_gc, d_contact, by = "record_id") |>
    mutate(name = coalesce(consent_name_eng, consent_name_span),
           gift_card_method = coalesce(consent_how_gift_card_eng, consent_how_gift_card_span)) |>
    select(-contains("eng"), -contains("span"))

  write_csv(d_gc_final, paste0("monthly_summaries/",lubridate::month(today(), label = T, abbr = F),"_gift_card_list.csv"))

}
