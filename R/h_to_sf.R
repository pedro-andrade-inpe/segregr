#' Convert local results of information theory index H to SF
#'
#' @param segregation_results a segreg object containing the results of a
#'     call to measure_segregation().
#'
#' @param bandwidths Attributes to be used. Default is all.
#'
#' @return a spatial sf object with local index H results
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
#' # export local index h results
#' index_h <- segregr::h_to_sf(segregation)
#'
#' tm_shape(index_h) +
#'   tm_polygons("h", n = 3) +
#'   tm_layout(legend.position=c(0.1, 0.1), scale = 0.5)
h_to_sf <- function(segregation_results, bandwidths = c()) {
  result <- segregation_results$areal_units %>%
    dplyr::left_join(segregation_results$h, by = "id") %>%
    dplyr::select(id, bw, h)

  if (length(bandwidths) != 0) {
    result <- filter(result, bw %in% bandwidths)
  }

  return(result)
}
