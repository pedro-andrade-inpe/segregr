#' Calculate segregation metrics
#'
#' @param data a spatial sf object of the study area divided into areal
#'      units (such as census tracts), containing the columns 'id', 'geometry',
#'      and one column per population group indicating that group's population in
#'      the local areal unit.
#' @param bandwidth numeric. A bandwidth, in meters, used to set the scale of
#'     analysis for spatial segregation measurement. When set to 0, aspatial
#'     metrics will be calculated.
#'
#' @return a segreg object, a list containing the input spatial data and the
#'     results of the segregation metrics.
#'
#' @export
#'
#' @examples
#'
#' library(sf)
#' library(segregr)
#'
#' # load sample data from package segregr
#' marilia_sf <- st_read(system.file("extdata/marilia_2010.gpkg", package = "segregr"))
#'
#' # calculate segregation metrics
#' segregation <- measure_segregation(marilia_sf)
#'
#' # global dissimilarity index
#' segregation$D
#'
#' # global entropy
#' segregation$E
#'
#' # global information theory index H
#' segregation$H
#'

measure_segregation <- function(data,
                                bandwidth = 0) {


  # This function calculates all segregation metrics available in the
  # segregr package and returns them all in a list containing individual values
  # of global metrics and data.frames of local metrics.
  #
  # This is the main function of the package, so it's quite long. In the future,
  # I should break this down into smaller functions. For now, the code below
  # is divided into 'chapters'


  # 1. Extract geometries ----------------------------------------------------

  ## sf object with areal units (census tracts) provided by the user
  areas_sf <- data %>%
    dplyr::select(id)

  ## extract the centroid of each areal unit
  locations_sf <- suppressWarnings(
    st_centroid(areas_sf)
  )

  # 2. Calculate distances between locations ---------------------------------

  distances_df <- st_distance(locations_sf, locations_sf) %>%
    tibble::as_tibble()

  colnames(distances_df) <- locations_sf$id
  distances_df$from <- locations_sf$id

  distances_df <- distances_df %>%
    tidyr::pivot_longer(-from, names_to = "to", values_to = "distance") %>%
    dplyr::mutate(distance = as.double(distance))

  # 3. Calculate Gaussian weights --------------------------------------------

  if (bandwidth == 0) {
    distances_df <- distances_df %>%
      dplyr::mutate(weight = dplyr::if_else(distance == 0, 1, 0))
  } else {
    distances_df <- distances_df %>%
      dplyr::mutate(weight = exp((-0.5) * (distance / bandwidth) * (distance / bandwidth)))
  }

  # 4. Extract population ----------------------------------------------------

  ## data.frame with the population in the study area, per group
  population_df <- data %>%
    st_set_geometry(NULL)

  ## use the order of the columns in the input data to order group names as factors
  group_names <- colnames(population_df)
  group_names <- group_names[group_names != "id"]

  ## convert population data.frame to long form, using group_names as factors for group column
  population_long_df <- population_df %>%
    tidyr::pivot_longer(-id, names_to = "group", values_to = "population") %>%
    dplyr::mutate(group = factor(group, levels = group_names))

  ## N = Total population of the study area ----
  N <- sum(population_long_df$population)

  ## Total population and proportion per group in the study area ----
  group_population_df <- population_long_df %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(total_population = sum(population), .groups = "drop") %>%
    dplyr::mutate(group_proportion_city = total_population / sum(total_population))

  # 5. Calculate Population Intensity ----------------------------------------

  distance_matrix <- expand.grid(from = locations_sf$id, to = locations_sf$id, group = group_names) %>%
    dplyr::left_join(population_long_df, by = c("from" = "id", "group")) %>%
    dplyr::left_join(population_long_df, by = c("to" = "id", "group"), suffix = c(".from", ".to")) %>%
    dplyr::left_join(distances_df, by = c("from", "to"))

  ## Calculate population intensity per group and locality
  intensity_df <- distance_matrix %>%
    dplyr::group_by(from, group) %>%
    dplyr::summarise(
      population = mean(population.from),
      population_intensity = weighted.mean(population.to, weight), .groups = "drop"
    ) %>%
    dplyr::rename(id = from)

  ## Calculate population intensity per locality
  localities_df <- distance_matrix %>%
    dplyr::group_by(from, to) %>%
    dplyr::summarise(
      population.from = sum(population.from),
      population.to = sum(population.to),
      distance = mean(distance),
      weight = mean(weight), .groups = "drop"
    ) %>%
    dplyr::group_by(from) %>%
    dplyr::summarise(
      population = mean(population.from),
      population_intensity = weighted.mean(population.to, weight), .groups = "drop"
    ) %>%
    dplyr::filter(population > 0) %>%
    dplyr::rename(id = from)


  # 6. Calculate Segregation Indices -----------------------------------------

  ## Dissimilarity Index ---------------------------------------------------

  ### I = Interaction Index, used in the Dissimilarity Index equation ----
  I <- intensity_df %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(population = sum(population), .groups = "drop") %>%
    dplyr::mutate(
      proportion = population / sum(population),
      inv_proportion = 1 - proportion,
      I = proportion * inv_proportion
    ) %>%
    dplyr::summarise(I = sum(I)) %>%
    .[[1]]


  ### Local Dissimilarity (d) ----
  local_dissimilarity_df <- intensity_df %>%
    dplyr::filter(population > 0) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
      population_locality = sum(population),
      group_proportion_locality = population_intensity / sum(population_intensity)
    ) %>%
    dplyr::left_join(group_population_df, by = "group") %>%
    dplyr::mutate(proportion_abs_diff = abs(group_proportion_locality - group_proportion_city)) %>%
    dplyr::mutate(dm = (population_locality / (2 * N * I)) * proportion_abs_diff) %>%
    dplyr::summarise(d = sum(dm), .groups = "drop")

  ### Global Dissimilarity (D) ----
  D <- sum(local_dissimilarity_df$d)


  ## Information Theory Indices (Entropy and H) ----------------------------

  ### Global Entropy (E) ----
  E <- group_population_df %>%
    dplyr::mutate(group_entropy = group_proportion_city * log(1 / group_proportion_city)) %>%
    dplyr::summarise(entropy = sum(group_entropy), .groups = "drop") %>%
    .[[1]]

  ### Local Entropy (e) ----
  local_entropy_df <- intensity_df %>%
    dplyr::filter(population_intensity > 0) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(proportion = population_intensity / sum(population_intensity)) %>%
    dplyr::mutate(group_entropy = proportion * log(1 / proportion)) %>%
    dplyr::summarise(
      population = sum(population),
      e = sum(group_entropy), .groups = "drop"
    )

  ### Local H Index (h) ----
  local_entropy_df <- local_entropy_df %>%
    dplyr::mutate(h = (population * (E - e)) / (E * N))

  ### Global H Index (H)
  H <- sum(local_entropy_df$h)


  ## Exposure and Isolation Indices (P and Q) ------------------------------
  iso_exp_df <- intensity_df %>%
    # dplyr::filter(population > 0) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(population_group_city = sum(population)) %>%
    dplyr::inner_join(localities_df, by = "id", suffix = c("", "_locality")) %>%
    dplyr::mutate(
      proportion_group_city = population / population_group_city,
      proportion_group_locality = population_intensity / population_intensity_locality
    ) %>%
    dplyr::select(id, group, proportion_group_city, proportion_group_locality) %>%
    dplyr::ungroup()

  iso_exp_matrix <- expand.grid(
    id = locations_sf$id,
    group_a = group_names,
    group_b = group_names
  )

  ### Local Exposure and Isolation
  local_iso_exp <- iso_exp_matrix %>%
    dplyr::left_join(iso_exp_df, by = c("id", "group_a" = "group")) %>%
    dplyr::left_join(iso_exp_df, by = c("id", "group_b" = "group"), suffix = c("_a", "_b")) %>%
    dplyr::mutate(isolation_exposure = proportion_group_city_a * proportion_group_locality_b) %>%
    tidyr::drop_na() %>%
    dplyr::select(id, group_a, group_b, isolation_exposure)

  ### Global Exposure and Isolation
  global_iso_exp <- local_iso_exp %>%
    dplyr::group_by(group_a, group_b) %>%
    dplyr::summarise(isolation_exposure = sum(isolation_exposure), .groups = "drop")


  # 7. Return results --------------------------------------------------------

  results <- list(
    areal_units = areas_sf,
    groups = group_names,
    bandwidth = bandwidth,
    D = D, # Global Dissimilarity
    E = E, # Global Entropy
    H = H, # Global H
    d = local_dissimilarity_df,
    h = local_entropy_df,
    # Global Exposure
    P = global_iso_exp %>% dplyr::filter(group_a != group_b) %>% dplyr::rename(exposure = isolation_exposure),
    # Global Isolation
    Q = global_iso_exp %>% dplyr::filter(group_a == group_b) %>% dplyr::select(group = group_a, isolation = isolation_exposure),
    # Local Exposure
    p = local_iso_exp %>% dplyr::filter(group_a != group_b) %>% dplyr::rename(exposure = isolation_exposure),
    # Local Isolation
    q = local_iso_exp %>% dplyr::filter(group_a == group_b) %>% dplyr::select(id,
      group = group_a,
      isolation = isolation_exposure
    )
  )

  return(results)
}
