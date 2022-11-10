#' Assign Neighborhoods
#'
#' Insert your redcap data and the output from geocoding to the tract level
#'
#' @param d Your redcap dataset with record_id's
#' @param d_tract Your geocoded to the tract level dataset
#'
#' @return Your redcap dataset with neighborhoods attached
#' @export
#'
#' @examples
#' d <- get_neighborhoods(d, d_tract)
#'
get_neighborhoods <- function(d, d_tract){
  d_tract <- d_tract |>
    select(record_id, 'fips_tract_id' = census_tract_id_2010) |>
    drop_na(fips_tract_id) |>
    mutate(fips_tract_id = as.character(fips_tract_id))

  d_tract_neigh <- left_join(d_tract, d_neigh)

  out <- left_join(d, d_tract_neigh, by = 'record_id')

  return(out)
}
