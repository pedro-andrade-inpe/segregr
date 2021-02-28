#' Calculate segregation metrics
#'
#' @param data an sf
#' @param bandwidth integer. A bandwidth, in meters, used for spatial segregation
#' @param indices character vector. A list of indices to compute
#'
#' @return a list with original data and results.
#' @export
#'
#' @examples
#'
measure_segregation <- function(data,
                                bandwidth = 0,
                                indices) {


  locations_df <- data %>%
    mutate(centroid = st_centroid(geometry)) %>%
    mutate(lon = map_dbl(centroid, function(x) x[[1]]),
           lat = map_dbl(centroid, function(x) x[[2]])) %>%
    select(id, lat, lon) %>%
    st_set_geometry(NULL)

  population_df <- data %>%
    st_set_geometry(NULL)

  population_long_df <- population_df %>%
    pivot_longer(-id, names_to = "group", values_to = "population")

  # Calculate distances between locations -----------------------------------
  distances_df <- geodist(locations_df, measure = "geodesic") %>%
    as_tibble()

  colnames(distances_df) <- locations_df$id
  distances_df$from <- locations_df$id

  distances_df <- distances_df %>%
    pivot_longer(-from, names_to = "to", values_to = "distance")

  ## Calculate weights - Gaussian --------------------------------------------
  if (bandwidth == 0) {
    distances_df <- distances_df %>%
      mutate(weight = if_else(distance == 0, 1, 0))
  } else {
    distances_df <- distances_df %>%
      mutate(weight = exp((-0.5) * (distance/bandwidth) * (distance/bandwidth)))
  }

  locations_matrix <- expand.grid(from = locations_df$id, to = locations_df$id)

  distance_matrix <- locations_matrix %>%
    left_join(population_long_df, by = c("from"="id")) %>%
    left_join(population_long_df, by = c("to"="id"), suffix = c(".from", ".to") ) %>%
    left_join(distances_df, by=c("from", "to"))

  intensity_df <- distance_matrix %>%
    filter(group.from == group.to) %>%
    group_by(from, group.from) %>%
    summarise(population = mean(population.from),
              population_intensity = weighted.mean(population.to, weight), .groups = "drop") %>%
    rename(group = group.from)

  localities_df <- distance_matrix %>%
    filter(group.from == group.to) %>%
    group_by(from, to) %>%
    summarise(population.from = sum(population.from),
              population.to = sum(population.to),
              distance = mean(distance),
              weight = mean(weight), .groups = "drop") %>%
    group_by(from) %>%
    summarise(population = mean(population.from),
              population_intensity = weighted.mean(population.to, weight), .groups = "drop") %>%
    filter(population > 0)

  # Dissimilarity Index -----------------------------------------------------
  I <- intensity_df %>%
    group_by(group) %>%
    summarise(population = sum(population), .groups = "drop") %>%
    mutate(proportion = population / sum(population),
           inv_proportion = 1 - proportion,
           I = proportion * inv_proportion) %>%
    summarise(I = sum(I)) %>%
    .[[1]]

  N <- sum(localities_df$population)

  groups_df <- population_long_df %>%
    group_by(group) %>%
    summarise(total_population = sum(population), .groups = "drop") %>%
    mutate(group_proportion_city = total_population / sum(total_population))

  dissimilarity_df <- intensity_df %>%
    filter(population > 0) %>%
    group_by(from) %>%
    mutate(population_locality = sum(population),
           group_proportion_locality = population_intensity / sum(population_intensity)) %>%
    left_join(groups_df, by = "group") %>%
    mutate(proportion_abs_diff = abs(group_proportion_locality - group_proportion_city)) %>%
    mutate(dm = (population_locality / (2 * N * I)) * proportion_abs_diff) %>%
    summarise(d = sum(dm), .groups = "drop")

  D <- sum(dissimilarity_df$d)

  # Entropy
  global_entropy <- groups_df %>%
    mutate(group_entropy = group_proportion_city * log(1 / group_proportion_city)) %>%
    summarise(entropy = sum(group_entropy), .groups = "drop") %>%
    .[[1]]

  local_entropy <- intensity_df %>%
    filter(population_intensity > 0) %>%
    group_by(from) %>%
    mutate(proportion = population_intensity / sum(population_intensity)) %>%
    mutate(group_entropy = proportion * log(1 / proportion)) %>%
    summarise(population = sum(population),
              local_entropy = sum(group_entropy), .groups = "drop")

  # H index
  local_entropy <- local_entropy %>%
    mutate(local_h = (population * (global_entropy - local_entropy)) / (global_entropy * N))

  H <- sum(local_entropy$local_h)

  # Isolation / Exposure
  iso_exp_df <- intensity_df %>%
    # filter(population > 0) %>%
    group_by(group) %>%
    mutate(population_group_city = sum(population)) %>%
    inner_join(localities_df, by = "from", suffix = c("", "_locality")) %>%
    mutate(proportion_group_city = population / population_group_city,
           proportion_group_locality = population_intensity / population_intensity_locality) %>%
    select(from, group, proportion_group_city, proportion_group_locality) %>%
    ungroup()

  iso_exp_df %>%
    mutate(isolation = proportion_group_city * proportion_group_locality) %>%
    group_by(group) %>%
    summarise(global_isolation = sum(isolation), .groups = "drop")

  iso_exp_matrix <- expand.grid(locality = locations_df$id,
                                group_a = groups_df$group,
                                group_b = groups_df$group)

  local_iso_exp <- iso_exp_matrix %>%
    left_join(iso_exp_df, by = c("locality"="from", "group_a"="group")) %>%
    left_join(iso_exp_df, by = c("locality"="from", "group_b"="group"), suffix = c("_a", "_b")) %>%
    mutate(isolation_exposure = proportion_group_city_a * proportion_group_locality_b) %>%
    drop_na() %>%
    select(locality, group_a, group_b, isolation_exposure)

  global_iso_exp <- local_iso_exp %>%
    group_by(group_a, group_b) %>%
    summarise(isolation_exposure = sum(isolation_exposure), .groups = "drop")

  results <- list(data = data,
                  D = D,
                  E = global_entropy,
                  H = H,
                  d = dissimilarity_df,
                  h = local_entropy,
                  P = global_iso_exp %>% filter(group_a == group_b) %>% select(group = group_a, isolation = isolation_exposure),
                  Q = global_iso_exp %>% filter(group_a != group_b) %>% rename(exposure = isolation_exposure),
                  p = local_iso_exp %>% filter(group_a == group_b) %>% select(locality,
                                                                              group = group_a,
                                                                              isolation = isolation_exposure),
                  q = local_iso_exp %>% filter(group_a != group_b) %>% rename(exposure = isolation_exposure)
                  )

  return(results)
}
