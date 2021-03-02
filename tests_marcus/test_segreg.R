library("dplyr")
library("ggplot2")
library("magrittr")
library("sf")
library("viridis")
library("devtools")

library("sf")
library("segregr")

# load sample data from package segregr
marilia_sf <- st_read(system.file("extdata/marilia_2010.gpkg", package = "segregr"))

# calculate segregation metrics
segregation <- measure_segregation(marilia_sf)

# global dissimilarity index
segregation$D

# global entropy
segregation$E

# global information theory index (Theil's H)
segregation$H



data <- marilia_sf
bandwidth = 0

segregation <- measure_segregation(marilia_sf, bandwidth = 1000)

segregation$d
segregation_results <- segregation

dissimilarity_to_sf(segregation) %>%
  ggplot() +
  geom_sf(aes(fill = dissimilarity)) +
  scale_fill_distiller(palette = "Spectral")

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

