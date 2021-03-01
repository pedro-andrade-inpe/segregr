#' Title
#'
#' @param segregation_results a SEGREG object containing the results of a
#'     call to measure_segregation().
#'
#' @return a spatial sf object with local isolation results
#' @export
#'
#' @examples
isolation_to_sf <- function(segregation_results) {
  return(
    segregation_results$areal_units %>%
      select(id, geometry) %>%
      left_join(segregation_results$q, by = c("id"))
  )
}
