dissimilarity_to_sf <- function(segregation_results) {

  return(
    segregation_results$data %>%
      select(id, geometry) %>%
      left_join(segregation_results$d, by=c("id"="from"))
  )

}
