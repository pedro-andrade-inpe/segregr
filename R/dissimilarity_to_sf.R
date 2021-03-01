dissimilarity_to_sf <- function(segregation_results) {
  return(
    segregation_results$areal_units %>%
      select(id, geometry) %>%
      left_join(segregation_results$d, by = c("id")) %>%
      rename(dissimilarity = d)
  )
}
