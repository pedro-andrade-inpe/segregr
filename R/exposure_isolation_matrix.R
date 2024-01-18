#' Convert global exposure and isolation to tabular form
#'
#' @param segregation_results a SEGREG object containing the results of a
#'     call to measure_segregation().
#'
#' @return a data.frame with global exposure and isolation results in tabular form
#' @export
#'
#' @examples
#' # load sample data from package segregr
#' marilia_sf <- sf::st_read(system.file("extdata/marilia_2010.gpkg", package = "segregr"))
#'
#' # calculate segregation metrics
#' segregation <- segregr::measure_segregation(marilia_sf)
#'
#' # convert to tabular form
#' segregr::exposure_isolation_matrix(segregation)
exposure_isolation_matrix <- function(segregation_results) {
  iso_exp <- rbind(
    segregation_results$Q %>% dplyr::select(group_a = group, group_b = group, iso_exp = isolation),
    segregation_results$P %>% dplyr::rename(iso_exp = exposure),
    fill = TRUE
  )

  return(
    iso_exp %>%
      tidyr::pivot_wider(names_from = group_b, values_from = iso_exp) %>%
      dplyr::rename(group = group_a)
  )
}
