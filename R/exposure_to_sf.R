exposure_to_sf <- function(segregation_results) {
  return(
    segregation_results$areal_units %>%
      select(id, geometry) %>%
      inner_join(segregation_results$p, by = c("id"))
  )
}
