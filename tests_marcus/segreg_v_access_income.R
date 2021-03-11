library(tidyverse)
library(sf)
library(aopdata)
library(viridis)
library(segregr)
library(h3jsr)

## load and prepare input data
rio_raw <- read_landuse(city = "rio", geometry = TRUE)

rio <- rio_raw %>%
  select(abbrev_muni, name_muni, id_hex, P001, R002, geometry) %>%
  filter(P001 > 0) %>%
  mutate(income_class = factor(R002,
                               levels = 1:5,
                               labels = c('E', 'D', 'C', 'B', 'A'))) %>%
  st_as_sf()

rio %>%
  ggplot() +
  geom_sf(aes(fill = income_class), color = NA) +
  scale_fill_brewer(palette = "Spectral", direction = -1)

rio_input <- rio %>%
  st_set_geometry(NULL) %>%
  select(id = id_hex, P001, income_class) %>%
  pivot_wider(names_from = income_class, values_from = P001, values_fill = 0)

rio_input$geometry <- h3jsr::h3_to_polygon(rio_input$id)
rio_input <- st_as_sf(rio_input)

## calculate segregation
rio_segreg <- measure_segregation(rio_input, bandwidths = c(0, 5000, 10000))

diss <- dissimilarity_to_sf(rio_segreg)

diss %>%
  ggplot() +
  geom_sf(aes(fill = dissimilarity), color = NA) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  facet_wrap(~bw) +
  theme_void() +
  theme(legend.position = "bottom")

iso <- isolation_to_sf(rio_segreg)

iso %>%
  filter(bw %in% c(5000)) %>%
  ggplot() +
  geom_sf(aes(fill = isolation), color = NA) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  facet_wrap(~group) +
  theme_void() +
  theme(legend.position = "bottom")

