library(tidyverse)
library(sf)
library(aopdata)
library(viridis)
library(segregr)

## load and prepare input data
rio_raw <- read_landuse(city = "rio", geometry = TRUE)

rio <- rio_raw %>%
  select(abbrev_muni, name_muni, id_hex, R001:, geometry) %>%
  pivot_longer(cols = white:black, names_to = "group", values_to = "population")


rio %>%
  filter(population > 0) %>%
  ggplot() +
  geom_sf(aes(fill = population, geometry = geometry), color = NA) +
  facet_wrap(~group) +
  scale_fill_distiller(palette = "Spectral") +
  theme_void() +
  theme(legend.position = "bottom")

rio_input <- rio_raw %>%
  select(id = id_hex, white = P002, black = P003, geometry) %>%
  filter(white + black > 0)

## calculate segregation
rio_segreg <- measure_segregation(rio_input, bandwidths = c(0, 700, 1000, 2000, 3000))

data <- rio_input
bandwidths <- c(0, 700, 1000, 2000, 3000)

diss <- dissimilarity_to_sf(rio_segreg)

diss %>%
  ggplot() +
  geom_sf(aes(fill = dissimilarity), color = NA) +
  scale_fill_viridis() +
  facet_wrap(~bw) +
  theme_void() +
  theme(legend.position = "bottom")

iso <- isolation_to_sf(rio_segreg)

iso %>%
  filter(bw %in% c(0, 700, 3000)) %>%
  ggplot() +
  geom_sf(aes(fill = isolation), color = NA) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  facet_grid(bw~group) +
  theme_void() +
  theme(legend.position = "bottom")

