exposure_to_sf <- function(segregation_results) {
  return(
    segregation_results$data %>%
      select(id, geometry) %>%
      left_join(segregation_results$q, by = c("id" = "locality"))
  )
}
