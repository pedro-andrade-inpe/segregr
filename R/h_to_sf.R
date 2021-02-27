h_to_sf <- function(segregation_results) {

  return(
    segregation_results$data %>%
      select(id, geometry) %>%
      left_join(segregation_results$h, by=c("id"="from")) %>%
      select(id, h = local_h, geometry)
  )

}
