isolation_to_sf <- function(segregation_results) {
  return(
    segregation_results$areal_units %>%
      select(id, geometry) %>%
      left_join(segregation_results$q, by = c("id"))
  )
}
