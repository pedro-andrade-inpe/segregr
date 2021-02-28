isolation_exposure_matrix <- function(segregation_results) {

  iso_exp <- rbind(segregation_results$Q %>% rename(iso_exp = exposure),
                   segregation_results$P %>% select(group_a = group, group_b = group, iso_exp = isolation))

  return(
    iso_exp %>%
      pivot_wider(names_from = group_b, values_from = iso_exp) %>%
      rename(group = group_a)
  )

}
