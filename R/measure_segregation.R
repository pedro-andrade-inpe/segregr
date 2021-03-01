#' Calculate segregation metrics
#'
#' @param data an sf
#' @param bandwidth integer. A bandwidth, in meters, used for spatial segregation
#'
#' @return a list with original data and results.
#' @export
#'
#' @examples
#'
measure_segregation <- function(data,
                                bandwidth = 0) {


  #' This function calculates all segregation metrics available in the
  #' segregr package and returns them all in a list containing individual values
  #' of global metrics and data.frames of local metrics.
  #'
  #' This is the main function of the package, so it's quite long. In the future,
  #' I should break this down into smaller functions. For now, the code below
  #' is divided into 'chapters'


# 1. Extract geometries ----------------------------------------------------

  ## sf object with areal units (census tracts) provided by the user
  areas_sf <- data %>%
    select(id, geometry)

  ## extract the centroid of each areal unit
  locations_sf <- areas_sf %>%
    mutate(centroid = st_centroid(geometry)) %>%
    mutate(
      lon = map_dbl(centroid, function(x) x[[1]]),
      lat = map_dbl(centroid, function(x) x[[2]])
    ) %>%
    select(id, lat, lon) %>%
    st_set_geometry(NULL) %>%
    mutate(geometry = map2(lon, lat, function(x, y) st_point(c(x, y)))) %>%
    st_as_sf()

  st_crs(locations_sf) <- st_crs(data)

# 2. Calculate distances between locations ---------------------------------

  distances_df <- st_distance(locations_sf, locations_sf) %>%
    as_tibble()

  colnames(distances_df) <- locations_sf$id
  distances_df$from <- locations_sf$id

  distances_df <- distances_df %>%
    pivot_longer(-from, names_to = "to", values_to = "distance") %>%
    mutate(distance = as.double(distance))

# 3. Calculate Gaussian weights --------------------------------------------

  if (bandwidth == 0) {
    distances_df <- distances_df %>%
      mutate(weight = if_else(distance == 0, 1, 0))
  } else {
    distances_df <- distances_df %>%
      mutate(weight = exp((-0.5) * (distance / bandwidth) * (distance / bandwidth)))
  }

# 4. Extract population ----------------------------------------------------

  ## data.frame with the population in the study area, per group
  population_df <- data %>%
    st_set_geometry(NULL)

  ## use the order of the columns in the input data to order group names as factors
  group_names <- colnames(population_df)
  group_names <- group_names[group_names != 'id']

  ## convert population data.frame to long form, using group_names as factors for group column
  population_long_df <- population_df %>%
    pivot_longer(-id, names_to = "group", values_to = "population") %>%
    mutate(group = factor(group, levels = group_names))

  ## N = Total population of the study area ----
  N <- sum(population_long_df$population)

  ## Total population and proportion per group in the study area ----
  group_population_df <- population_long_df %>%
    group_by(group) %>%
    summarise(total_population = sum(population), .groups = "drop") %>%
    mutate(group_proportion_city = total_population / sum(total_population))

# 5. Calculate Population Intensity ----------------------------------------

  distance_matrix <- expand.grid(from = locations_sf$id, to = locations_sf$id, group = group_names) %>%
      left_join(population_long_df, by = c("from" = "id", "group")) %>%
      left_join(population_long_df, by = c("to" = "id", "group"), suffix = c(".from", ".to")) %>%
      left_join(distances_df, by = c("from", "to"))

  ## Calculate population intensity per group and locality
  intensity_df <- distance_matrix %>%
    group_by(from, group) %>%
    summarise(
      population = mean(population.from),
      population_intensity = weighted.mean(population.to, weight), .groups = "drop"
    ) %>%
    rename(id = from)

  ## Calculate population intensity per locality
  localities_df <- distance_matrix %>%
    group_by(from, to) %>%
    summarise(
      population.from = sum(population.from),
      population.to = sum(population.to),
      distance = mean(distance),
      weight = mean(weight), .groups = "drop"
    ) %>%
    group_by(from) %>%
    summarise(
      population = mean(population.from),
      population_intensity = weighted.mean(population.to, weight), .groups = "drop"
    ) %>%
    filter(population > 0) %>%
    rename(id = from)


# 6. Calculate Segregation Indices -----------------------------------------

  ## Dissimilarity Index ---------------------------------------------------

  ### I = Interaction Index, used in the Dissimilarity Index equation ----
  I <- intensity_df %>%
    group_by(group) %>%
    summarise(population = sum(population), .groups = "drop") %>%
    mutate(
      proportion = population / sum(population),
      inv_proportion = 1 - proportion,
      I = proportion * inv_proportion
    ) %>%
    summarise(I = sum(I)) %>%
    .[[1]]


  ### Local Dissimilarity (d) ----
  local_dissimilarity_df <- intensity_df %>%
    filter(population > 0) %>%
    group_by(id) %>%
    mutate(
      population_locality = sum(population),
      group_proportion_locality = population_intensity / sum(population_intensity)
    ) %>%
    left_join(group_population_df, by = "group") %>%
    mutate(proportion_abs_diff = abs(group_proportion_locality - group_proportion_city)) %>%
    mutate(dm = (population_locality / (2 * N * I)) * proportion_abs_diff) %>%
    summarise(d = sum(dm), .groups = "drop")

  ### Global Dissimilarity (D) ----
  D <- sum(local_dissimilarity_df$d)


  ## Information Theory Indices (Entropy and H) ----------------------------

  ### Global Entropy (E) ----
  E <- group_population_df %>%
    mutate(group_entropy = group_proportion_city * log(1 / group_proportion_city)) %>%
    summarise(entropy = sum(group_entropy), .groups = "drop") %>%
    .[[1]]

  ### Local Entropy (e) ----
  local_entropy_df <- intensity_df %>%
    filter(population_intensity > 0) %>%
    group_by(id) %>%
    mutate(proportion = population_intensity / sum(population_intensity)) %>%
    mutate(group_entropy = proportion * log(1 / proportion)) %>%
    summarise(
      population = sum(population),
      e = sum(group_entropy), .groups = "drop"
    )

  ### Local H Index (h) ----
  local_entropy_df <- local_entropy_df %>%
    mutate(h = (population * (E - e)) / (E * N))

  ### Global H Index (H)
  H <- sum(local_entropy$h)


  ## Exposure and Isolation Indices (P and Q) ------------------------------
  iso_exp_df <- intensity_df %>%
    # filter(population > 0) %>%
    group_by(group) %>%
    mutate(population_group_city = sum(population)) %>%
    inner_join(localities_df, by = "id", suffix = c("", "_locality")) %>%
    mutate(
      proportion_group_city = population / population_group_city,
      proportion_group_locality = population_intensity / population_intensity_locality
    ) %>%
    select(id, group, proportion_group_city, proportion_group_locality) %>%
    ungroup()

  iso_exp_matrix <- expand.grid(
    id = locations_sf$id,
    group_a = group_names,
    group_b = group_names
  )

  ### Local Exposure and Isolation
  local_iso_exp <- iso_exp_matrix %>%
    left_join(iso_exp_df, by = c("id", "group_a" = "group")) %>%
    left_join(iso_exp_df, by = c("id", "group_b" = "group"), suffix = c("_a", "_b")) %>%
    mutate(isolation_exposure = proportion_group_city_a * proportion_group_locality_b) %>%
    drop_na() %>%
    select(id, group_a, group_b, isolation_exposure)

  ### Global Exposure and Isolation
  global_iso_exp <- local_iso_exp %>%
    group_by(group_a, group_b) %>%
    summarise(isolation_exposure = sum(isolation_exposure), .groups = "drop")


# 7. Return results --------------------------------------------------------

  results <- list(
    areal_units = areas_sf,
    groups = group_names,
    D = D, # Global Dissimilarity
    E = E, # Global Entropy
    H = H, # Global H
    d = local_dissimilarity_df,
    h = local_entropy_df,
    # Global Exposure
    P = global_iso_exp %>% filter(group_a != group_b) %>% rename(q = isolation_exposure),
    # Global Isolation
    Q = global_iso_exp %>% filter(group_a == group_b) %>% select(group = group_a, p = isolation_exposure),
    # Local Exposure
    p = local_iso_exp %>% filter(group_a != group_b) %>% rename(exposure = isolation_exposure),
    # Local Isolation
    q = local_iso_exp %>% filter(group_a == group_b) %>% select(id,
                                                                group = group_a,
                                                                isolation = isolation_exposure
    )
  )

  return(results)
}
