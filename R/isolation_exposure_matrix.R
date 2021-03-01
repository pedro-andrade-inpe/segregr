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
