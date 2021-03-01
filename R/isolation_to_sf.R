isolation_to_sf <- function(segregation_results) {
  return(
    segregation_results$data %>%
      select(id, geometry) %>%
      left_join(segregation_results$p, by = c("id" = "locality"))
  )
}
