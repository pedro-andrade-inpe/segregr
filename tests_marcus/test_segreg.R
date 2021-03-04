library("dplyr")
library("ggplot2")
library("magrittr")
library("sf")
library("viridis")
library("devtools")
library("data.table")
library("sf")
library("segregr")
library("tidyverse")
library("geobr")

gla_sf <- st_read(here::here("inst/extdata/gla.gpkg"))
system.time(segregation <- measure_segregation(gla_sf, bandwidth = 2000))
# com data.table =  4.093 segundos
# sem data.table = 36.683 segundos

# load sample data from package segregr
marilia_sf <- st_read(system.file("extdata/marilia_2010.gpkg", package = "segregr"))

data <- gla_sf
bandwidths <- 1000
bandwidths <- c(0, 500, 1000, 2000)

# calculate segregation metrics
system.time(segregation <- measure_segregation(marilia_sf, bandwidths = c(0, 500, 1000, 2000, 5000)))

## dplyr version of measure_segregation: 3.824 seconds
## data.table version .................:


# global dissimilarity index
segregation$D %>%
  ggplot(aes(x=bw, y=D)) + geom_path() + geom_point()

# global entropy
segregation$E

# global information theory index (Theil's H)
segregation$H %>%
  ggplot(aes(x=bw, y=H)) + geom_path() + geom_point()



data <- marilia_sf
bandwidth = 0

segregation <- measure_segregation(marilia_sf, bandwidth = 0)

segregation$d
segregation_results <- segregation

dissimilarity_to_sf(segregation) %>%
  ggplot() +
  geom_sf(aes(fill = dissimilarity)) +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(~bw)

h_to_sf(segregation) %>%
  ggplot() +
  geom_sf(aes(fill = h)) +
  scale_fill_viridis(direction = -1) +
  facet_wrap(~bw)

entropy_to_sf(segregation) %>%
  ggplot() +
  geom_sf(aes(fill = entropy)) +
  scale_fill_viridis() +
  facet_wrap(~bw)

isolation_to_sf(segregation) %>% filter(group == "Black") %>%
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

exposure_isolation_matrix(segregation)

poa <- geobr::lookup_muni("Porto Alegre")

poa

poa_census <- geobr::read_census_tract(code_tract = 43)

library(geobr)
poa_grid <- geobr::read_statistical_grid(code_grid = "RS")

poa_grid %>%
  ggplot() +
  geom_sf()

st_write(poa_grid, "poa.gpkg")



b <- 2000
d <- 1:10000
weight = exp((-0.5) * (d / b) * (d / b))
data <- tibble::tibble(distance = d, weight = weight)
ggplot(data %>% filter(weight >= 0.01)) + geom_point(aes(x=distance, y= weight)) + geom_vline(xintercept = b)

