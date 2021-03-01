#' Title
#'
#' @param segregation_results a SEGREG object containing the results of a
#'     call to measure_segregation().
#'
#' @return a data.frame with global exposure and isolation results in tabular form
#' @export
#'
#' @examples
isolation_exposure_matrix <- function(segregation_results) {
  iso_exp <- rbind(
    segregation_results$Q %>% select(group_a = group, group_b = group, iso_exp = isolation),
    segregation_results$P %>% rename(iso_exp = exposure)
  )

  return(
    iso_exp %>%
      pivot_wider(names_from = group_b, values_from = iso_exp) %>%
      rename(group = group_a)
  )
}
