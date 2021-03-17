#' Title
#'
#' @param segregation_results a segreg object containing the results of a
#'     call to measure_segregation().
#' @param bandwidths
#'
#' @return
#' @export
#'
#' @examples
local_metrics_to_sf <- function(segregation_results, bandwidths = c()) {

  # prepare global results dataframe ----------------------------------------
  # exposure / isolation
  segreg_iso <- segregation_results$q %>%
    dplyr::mutate(group_a = group, group_b = group) %>%
    dplyr::mutate(groups = paste(group_a, group_b),
                  metric = paste("iso", group_a, group_b, sep = "_")) %>%
    dplyr::select(bw, id, groups, metric, value = isolation)

  segreg_exp <- segregation_results$p %>%
    dplyr::mutate(groups = paste(group_a, group_b),
                  metric = paste("exp", group_a, group_b, sep = "_")) %>%
    dplyr::select(bw, id, groups, metric, value = exposure)

  iso_exp <- rbind(segreg_iso, segreg_exp) %>%
    dplyr::arrange(bw, id, groups) %>%
    dplyr::select(-groups) %>%
    tidyr::pivot_wider(names_from = metric, values_from = value)

  # dissimilarity, entropy, and H
  segreg_results <- segregation_results$d %>%
    dplyr::left_join(segregation_results$h, by = c("bw", "id")) %>%
    dplyr::select(-population)

  names(segreg_results) <- c("id", "bw", "dissimilarity", "entropy", "h")

  segreg_results <- segreg_results %>%
    dplyr::left_join(iso_exp)

  if (length(bandwidths) != 0) {
    segreg_results <- dplyr::filter(segreg_results, bw %in% bandwidths)
  }

  segreg_results <- segregation_results$areal_units %>%
    dplyr::select(id) %>%
    dplyr::left_join(segreg_results, by = c("id"))

  return(segreg_results)
}
