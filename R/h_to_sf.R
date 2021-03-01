#' Title
#'
#' @param segregation_results a SEGREG object containing the results of a
#'     call to measure_segregation().
#'
#' @return a spatial sf object with local results of Theil's index H
#' @export
#'
#' @examples
h_to_sf <- function(segregation_results) {
  return(
    segregation_results$areal_units %>%
      select(id, geometry) %>%
      left_join(segregation_results$h, by = c("id")) %>%
      select(id, h, geometry)
  )
}
