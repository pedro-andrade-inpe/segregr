calculate_distances <- function(points, method = "geodist") {
  # points: an sf with an 'id' field and geometry

  # Distances are only relevant when using spatial segregation metrics.
  # Hence, if bandwidth == 0, then this calculation can be ignored.

  distances <- NULL
  if (method == "sf") {
    distances <- calculate_distances_sf(points)
  } else {
    # geodist
    distances <- calculate_distances_geodist(points)
  }

  distances <- data.table::as.data.table(distances)

  colnames(distances) <- points$id
  distances$from <- points$id

  distances <- data.table::melt(
    distances,
    id.vars = "from",
    variable.name = "to",
    value.name = "distance"
  )

  distances[, distance := as.double(distance)]

  return(distances)
}

calculate_distances_sf <- function(points) {
  distances <- st_distance(points, points)

  return(distances)
}

calculate_distances_geodist <- function(points) {
  points_latlon <- suppressWarnings(
    sf::st_centroid(points) %>%
      sf::st_transform(4326) %>%
      sf::st_coordinates()
  )

  distances <- geodist::geodist(points_latlon, points_latlon)

  return(distances)
}

calculate_gaussian_weights <- function(distances, bandwidths, cutoff = 0.01) {
  weights <- purrr::map(bandwidths, function(b) {
    if (b == 0) {
      dplyr::mutate(distances, bw = b,  weight = dplyr::if_else(distance == 0, 1, 0))

      # distances_df[, weight = data.table::fifelse(distance == 0, 1, 0)]
    } else {
      dplyr::mutate(distances, bw = b, weight = exp((-0.5) * (distance / b) * (distance / b)))

      # distances_df[, weight := exp((-0.5) * (distance / b) * (distance / b))]
    }
  })

  weights <- data.table::rbindlist(weights)

  # remove localities with too little influence
  weights <- subset(weights, weight >= 0.01)
  return(weights)
}

assign_population <- function(weights, groups, population) {
    groups_dt <- data.table::data.table(group = groups, k = 1)
    weights[, k := 1]

    dm <- merge(weights, groups_dt, allow.cartesian = TRUE)

    ## grab population counts
    dm[population, on = .(from = id, group), population.from := i.population]
    dm[population, on = .(to = id, group), population.to := i.population]

    return(dm)
}
