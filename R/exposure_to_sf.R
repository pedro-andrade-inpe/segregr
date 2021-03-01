#' Title
#'
#' @param segregation_results a SEGREG object containing the results of a
#'     call to measure_segregation().
#'
#' @return a spatial sf object with local exposure results
#' @export
#'
#' @examples
exposure_to_sf <- function(segregation_results) {
  return(
    segregation_results$areal_units %>%
      select(id, geometry) %>%
      inner_join(segregation_results$p, by = c("id"))
  )
}
