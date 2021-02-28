library("tidyverse")
library("sf")
library("segregR")
library("geodist")
library("viridis")
library("devtools")

marilia_sf <- st_read(here::here("inst/extdata", "marilia_2010.gpkg")) %>%
  rename(geometry = geom)

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
  facet_wrap(~group)

exposure_to_sf(segregation) %>%
  filter(group_a %in% c("mw_05_to_1", "mw_20_above"),
         group_b %in% c("mw_05_to_1", "mw_20_above")) %>%
  ggplot() +
  geom_sf(aes(fill = exposure), colour = NA) +
  scale_fill_viridis() +
  facet_grid(group_a~group_b)

isolation_exposure_matrix(segregation)

