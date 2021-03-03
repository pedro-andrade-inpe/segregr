#' Convert local dissimilarity results to SF
#'
#' @param segregation_results a segreg object containing the results of a
#'     call to measure_segregation().
#'
#' @return a spatial sf object with local dissimilarity results
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
#' # export local dissimilarity results
#' dissimilarity <- dissimilarity_to_sf(segregation)
#'
#' # plot local dissimilarity
#' ggplot(data = dissimilarity) +
#'   geom_sf(aes(fill = dissimilarity)) +
#'   scale_fill_distiller(palette = "Spectral")
dissimilarity_to_sf <- function(segregation_results, bandwidths = c()) {

  result <- segregation_results$areal_units %>%
    dplyr::left_join(segregation_results$d, by = c("id")) %>%
    dplyr::select(id, bw, dissimilarity = d)

  if (length(bandwidths) != 0) {
    result <- filter(result, bw %in% bandwidths)
  }

  return(result)
}
