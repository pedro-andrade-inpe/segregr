#' Convert local isolation results to SF
#'
#' @param segregation_results a segreg object containing the results of a
#'     call to measure_segregation().
#'
#' @return a spatial sf object with local isolation results
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
#' # export local isolation results
#' isolation <- isolation_to_sf(segregation)
#'
#' # plot local isolation
#' ggplot(data = isolation) +
#'   geom_sf(aes(fill = isolation)) +
#'   scale_fill_distiller(palette = "Spectral") +
#'   facet_wrap(~group) +
#'   theme_void()
#'

isolation_to_sf <- function(segregation_results) {
  return(
    segregation_results$areal_units %>%
      dplyr::select(id) %>%
      dplyr::left_join(segregation_results$q, by = c("id"))
  )
}
