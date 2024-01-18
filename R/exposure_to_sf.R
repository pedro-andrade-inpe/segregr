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
#' # load sample data from package segregr
#' marilia_sf <- sf::st_read(system.file("extdata/marilia_2010.gpkg", package = "segregr"))
#'
#' # calculate segregation metrics
#' segregation <- segregr::measure_segregation(marilia_sf)
#'
#' # export local isolation results
#' exposure <- segregr::exposure_to_sf(segregation)
#'
#' # plot local exposure
#' #ggplot(data = exposure) +
#'#   geom_sf(aes(fill = exposure), color = NA) +
#' #  scale_fill_distiller(palette = "Spectral") +
#'#   facet_grid(group_a ~ group_b) +
#' #  theme_void()
exposure_to_sf <- function(segregation_results) {
  return(
    segregation_results$areal_units %>%
      dplyr::select(id) %>%
      dplyr::inner_join(segregation_results$p, by = c("id"))
  )
}
