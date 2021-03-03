#' Convert local results of information theory index H to SF
#'
#' @param segregation_results a segreg object containing the results of a
#'     call to measure_segregation().
#'
#' @return a spatial sf object with local index H results
#'
#' @export
#'
#' @examples
#'
#' library("sf")
#' library("ggplot2")
#' library("segregr")
#'
#' # load sample data from package segregr
#' marilia_sf <- st_read(system.file("extdata/marilia_2010.gpkg", package = "segregr"))
#'
#' # calculate segregation metrics
#' segregation <- measure_segregation(marilia_sf)
#'
#' # export local index h results
#' index_h <- h_to_sf(segregation)
#'
#' # plot local index h
#' ggplot(data = index_h) +
#'   geom_sf(aes(fill = h)) +
#'   scale_fill_distiller(palette = "Spectral")
h_to_sf <- function(segregation_results, bandwidths = c()) {

  result <- segregation_results$areal_units %>%
    dplyr::left_join(segregation_results$h, by = c("id")) %>%
    dplyr::select(id, bw, h)

  if (length(bandwidths) != 0) {
    result <- filter(result, bw %in% bandwidths)
  }

  return(result)
}
