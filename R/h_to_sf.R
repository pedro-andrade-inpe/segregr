h_to_sf <- function(segregation_results) {
  return(
    segregation_results$areal_units %>%
      select(id, geometry) %>%
      left_join(segregation_results$h, by = c("id")) %>%
      select(id, h, geometry)
  )
}
