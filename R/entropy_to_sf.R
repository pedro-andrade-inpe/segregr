#' Convert local entropy results to SF
#'
#' @param segregation_results a segreg object containing the results of a
#'     call to measure_segregation().
#' @param bandwidths Bands to be used.
#'
#' @return A spatial sf object with local entropy results.
#'
#' @export
#'
#' @examples
#' # load sample data from package segregr
#' marilia_sf <- sf::st_read(system.file("extdata/marilia_2010.gpkg", package = "segregr"))
#'
#' # calculate segregation metrics
#' segregation <- segregr::measure_segregation(marilia_sf)
#'
#' # export local entropy results
#' entropy <- segregr::entropy_to_sf(segregation)
#'
#' # plot local entropy
#' #ggplot(data = entropy) +
#'  # geom_sf(aes(fill = entropy)) +
#'  # scale_fill_distiller(palette = "Spectral")
entropy_to_sf <- function(segregation_results, bandwidths = c()) {

  result <- segregation_results$areal_units %>%
    dplyr::left_join(segregation_results$h, by = c("id")) %>%
    dplyr::select(id, bw, entropy = e)

  if (length(bandwidths) != 0) {
    result <- filter(result, bw %in% bandwidths)
  }

  return(result)
}
