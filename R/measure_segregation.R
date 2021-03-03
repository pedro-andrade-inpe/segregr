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
measure_segregation <- function(data,
                                bandwidths = 0) {


  # This function calculates all segregation metrics available in the
  # segregr package and returns them all in a list containing individual values
  # of global metrics and data.frames of local metrics.
  #
  # This is the main function of the package, so it's quite long. In the future,
  # I should break this down into smaller functions. For now, the code below
  # is divided into 'chapters'


  # 1. Extract geometries ----------------------------------------------------

  ## sf object with areal units (census tracts) provided by the user
  areas_sf <- data["id"]

  ## extract the centroid of each areal unit
  locations_sf <- suppressWarnings(
    sf::st_centroid(areas_sf)
  )

  locations_latlon <- suppressWarnings(
    sf::st_centroid(areas_sf) %>%
      sf::st_transform(4326) %>%
      sf::st_coordinates()
  )

  # 4. Extract population ----------------------------------------------------

  ## data.frame with the population in the study area, per group
  population_df <- st_set_geometry(data, NULL)
  data.table::setDT(population_df)

  ## use the order of the columns in the input data to order group names as factors
  group_names <- colnames(population_df)
  group_names <- group_names[group_names != "id"]

  ## convert population data.frame to long form, using group_names as factors for group column
  population_long_df <- data.table::melt(population_df,
    id.vars = "id",
    variable.name = "group",
    value.name = "population"
  )

  population_long_df[, group := factor(group, levels = group_names)]

  ## N = Total population of the study area ----
  N <- sum(population_long_df$population)

  ## Total population and proportion per group in the study area ----
  group_population_df <- population_long_df[, .(total_population = sum(population)), by = group]
  group_population_df[, group_proportion_city := total_population / sum(total_population)]

  # 2. Calculate distances between locations ---------------------------------
  distances_df <- calculate_distances(locations_sf, "sf")

  # 3. Calculate Gaussian weights by bandwidth -----------------------------
  weights_df <- calculate_gaussian_weights(distances_df, bandwidths)


  # 5. Calculate Population Intensity ----------------------------------------
  distance_matrix <- calculate_distance_matrix(
    locations_sf$id, group_names, bandwidths,
    population_long_df, weights_df
  )

  ## Calculate population intensity per group and locality
  intensity_df <- distance_matrix[, .(
    population = mean(population.from),
    population_intensity = weighted.mean(population.to, weight)
  ),
  by = .(from, group, bw)
  ]
  data.table::setnames(intensity_df, "from", "id")

  ## Calculate population intensity per locality
  localities_df <- distance_matrix[, .(
    population.from = sum(population.from),
    population.to = sum(population.to),
    distance = mean(distance),
    weight = mean(weight)
  ),
  by = .(from, to, bw)
  ]
  localities_df <- localities_df[, .(
    population = mean(population.from),
    population_intensity = weighted.mean(population.to, weight)
  ),
  by = .(from, bw)
  ]
  data.table::setnames(localities_df, old = "from", new = "id")

  # 6. Calculate Segregation Indices -----------------------------------------

  ## Dissimilarity Index ---------------------------------------------------

  ### I = Interaction Index, used in the Dissimilarity Index equation ----
  I <- population_long_df[, .(population = sum(population)), by = group]
  I[, proportion := population / sum(population)]
  I[, inv_proportion := 1 - proportion]
  I[, partial_I := proportion * inv_proportion]

  I <- sum(I$partial_I)

  ### Local Dissimilarity (d) ----
  local_dissimilarity_df <- intensity_df

  local_dissimilarity_df[,
    `:=`(
      population_locality = sum(population),
      group_proportion_locality = population_intensity / sum(population_intensity)
    ),
    by = .(id, bw)
  ]

  local_dissimilarity_df[group_population_df,
    on = "group",
    group_proportion_city := i.group_proportion_city
  ]

  local_dissimilarity_df[, proportion_abs_diff := abs(group_proportion_locality - group_proportion_city)]
  local_dissimilarity_df[, dm := (population_locality / (2 * N * I)) * proportion_abs_diff]
  local_dissimilarity_df <- local_dissimilarity_df[, .(d = sum(dm)), by = .(id, bw)]

  ### Global Dissimilarity (D) ----
  D <- local_dissimilarity_df[, .(D = sum(d, na.rm = TRUE)), by = bw]

  ## Information Theory Indices (Entropy and H) ----------------------------

  ### Global Entropy (E) ----
  E <- group_population_df[, .(group_entropy = group_proportion_city * log(1 / group_proportion_city))]
  E <- sum(E$group_entropy)

  ### Local Entropy (e) ----
  local_entropy_df <- intensity_df
  local_entropy_df[, proportion := population_intensity / sum(population_intensity), by = .(id, bw)]
  local_entropy_df[, group_entropy := proportion * log(1 / proportion)]
  local_entropy_df <- local_entropy_df[, .(
    population = sum(population),
    e = sum(group_entropy, na.rm = TRUE)
  ), by = .(id, bw)]

  ### Local H Index (h) ----
  local_entropy_df[, h := (population * (E - e)) / (E * N)]

  ### Global H Index (H)
  H <- local_entropy_df[, .(H = sum(h, na.rm = TRUE)), by = bw]

  ## Exposure and Isolation Indices (P and Q) ------------------------------
  iso_exp_df <- intensity_df
  iso_exp_df[, population_group_city := sum(population), by = group]
  iso_exp_df[localities_df, on = .(id, bw), population_intensity_locality := i.population_intensity]
  iso_exp_df[, `:=`(
    proportion_group_city = population / population_group_city,
    proportion_group_locality = population_intensity / population_intensity_locality
  )]
  iso_exp_df <- iso_exp_df[, .(id, bw, group, proportion_group_city, proportion_group_locality)]

  iso_exp_matrix <- expand.grid(
    id = locations_sf$id,
    bw = bandwidths,
    group_a = group_names,
    group_b = group_names
  ) %>% data.table::setDT()

  ### Local Exposure and Isolation
  iso_exp_matrix[iso_exp_df,
    on = .(id, bw, group_a = group),
    `:=`(
      proportion_group_city_a = i.proportion_group_city,
      proportion_group_locality_a = i.proportion_group_locality
    )
  ]

  iso_exp_matrix[iso_exp_df,
    on = .(id, bw, group_b = group),
    `:=`(
      proportion_group_city_b = i.proportion_group_city,
      proportion_group_locality_b = i.proportion_group_locality
    )
  ]

  iso_exp_matrix[, isolation_exposure := proportion_group_city_a * proportion_group_locality_b]
  local_iso_exp <- iso_exp_matrix[, .(id, bw, group_a, group_b, isolation_exposure)]

  ### Global Exposure and Isolation
  global_iso_exp <- local_iso_exp[,
    .(isolation_exposure = sum(isolation_exposure, na.rm = TRUE)),
    by = .(bw, group_a, group_b)
  ]

  #### Separate Exposure and Isolation data
  global_exposure <- global_iso_exp[group_a != group_b]
  data.table::setnames(global_exposure, old = "isolation_exposure", new = "exposure")

  global_isolation <- global_iso_exp[group_a == group_b, .(bw, group_a, isolation_exposure)]
  data.table::setnames(global_isolation,
    old = c("group_a", "isolation_exposure"),
    new = c("group", "isolation")
  )

  local_exposure <- local_iso_exp[group_a != group_b]
  data.table::setnames(local_exposure,
    old = "isolation_exposure",
    new = "exposure"
  )

  local_isolation <- local_iso_exp[group_a == group_b, .(id, bw, group_a, isolation_exposure)]
  data.table::setnames(local_isolation,
    old = c("group_a", "isolation_exposure"),
    new = c("group", "isolation")
  )

  # 7. Return results --------------------------------------------------------

  results <- list(
    areal_units = areas_sf,
    groups = group_names,
    bandwidth = bandwidths,
    D = D, # Global Dissimilarity
    E = E, # Global Entropy
    H = H, # Global H
    d = local_dissimilarity_df,
    h = local_entropy_df,
    # Global Exposure
    P = global_exposure,
    # Global Isolation
    Q = global_isolation,
    # Local Exposure
    p = local_exposure,
    # Local Isolation
    q = local_isolation
  )

  return(results)
}
