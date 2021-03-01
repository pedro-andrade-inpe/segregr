library("dplyr")
library("ggplot")
library("magrittr")
library("sf")
library("viridis")
library("devtools")


marilia_sf <- st_read(system.file("extdata/marilia_2010.gpkg", package = "segregr"))

data <- marilia_sf
bandwidth = 0

segregation <- measure_segregation(marilia_sf, bandwidth = 1000)

segregation$d
segregation_results <- segregation

dissimilarity_to_sf(segregation) %>%
  ggplot() +
  geom_sf(aes(fill = dissimilarity)) +
  scale_fill_viridis()

h_to_sf(segregation) %>%
  ggplot() +
  geom_sf(aes(fill = h)) +
  scale_fill_viridis(direction = -1)

entropy_to_sf(segregation) %>%
  ggplot() +
  geom_sf(aes(fill = entropy)) +
  scale_fill_viridis()

isolation_to_sf(segregation) %>%
  ggplot() +
  geom_sf(aes(fill = isolation), colour = NA) +
  scale_fill_viridis() +
  facet_wrap(~group) +
  theme_void()

exposure_to_sf(segregation) %>%
  ggplot() +
  geom_sf(aes(fill = exposure), colour = NA) +
  scale_fill_viridis() +
  facet_grid(group_a~group_b) +
  theme_void()

isolation_exposure_matrix(segregation)

