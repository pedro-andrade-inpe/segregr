#' Title
#'
#' @param segregation_results
#' @param bandwidths
#'
#' @return
#' @export
#'
#' @examples
global_metrics_to_df <- function(segregation_results, bandwidths = c()) {

  # prepare global results dataframe ----------------------------------------
  # exposure / isolation
  segreg_iso <- segregation_results$Q %>%
    dplyr::mutate(group_a = group, group_b = group) %>%
    dplyr::mutate(groups = paste(group_a, group_b),
                  metric = paste("iso", group_a, group_b, sep = "_")) %>%
    dplyr::select(bw, groups, metric, value = isolation)

  segreg_exp <- segregation_results$P %>%
    dplyr::mutate(groups = paste(group_a, group_b),
                  metric = paste("exp", group_a, group_b, sep = "_")) %>%
    dplyr::select(bw, groups, metric, value = exposure)

  iso_exp <- rbind(segreg_iso, segreg_exp) %>%
    dplyr::arrange(bw, groups) %>%
    dplyr::select(-groups) %>%
    tidyr::pivot_wider(names_from = metric, values_from = value)

  # dissimilarity, entropy, and H
  segreg_results <- segregation_results$D
  segreg_results$E <- segregation_results$E
  segreg_results$H <- segregation_results$H$H

  names(segreg_results) <- c("bw", "dissimilarity", "entropy", "h")

  segreg_results <- segreg_results %>%
    dplyr::left_join(iso_exp)

  if (length(bandwidths) != 0) {
    segreg_results <- dplyr::filter(segreg_results, bw %in% bandwidths)
  }

  return(segreg_results)

}
