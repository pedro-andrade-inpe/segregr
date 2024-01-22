#' Convert local exposure results to SF
#'
#' @param segregation_results a segreg object containing the results of a
#'     call to measure_segregation().
#'
#' @return a spatial sf object with local exposure results
#'
#' @export
#'
#' @examples
#' require(tmap)
#' # load sample data from package segregr
#' marilia_sf <- sf::st_read(system.file("extdata/marilia_2010.gpkg", package = "segregr"))
#'
#' # calculate segregation metrics
#' segregation <- segregr::measure_segregation(marilia_sf)
#'
#' # export local isolation results
#' exposure <- segregr::exposure_to_sf(segregation)
#'
#' #plot local exposure
#' tm_shape(exposure) +
#'   tm_polygons("exposure", n = 3) +
#'   tm_layout(legend.position=c(0.1, 0.1), scale = 0.5)
exposure_to_sf <- function(segregation_results) {
  segregation_results$areal_units %>%
    dplyr::select(id) %>%
    dplyr::inner_join(segregation_results$p, by = "id")
}
