entropy_to_sf <- function(segregation_results) {

  return(
    segregation_results$data %>%
      select(id, geometry) %>%
      left_join(segregation_results$h, by=c("id"="from")) %>%
      select(id, entropy = local_entropy, geometry)
  )

}
