library("viridis")
library("data.table")
library("sf")
library("tidyverse")
library("geobr")
devtools::load_all(".")

# gla_sf <- st_read(here::here("inst/extdata/gla.gpkg"))
# system.time(segregation <- measure_segregation(gla_sf, bandwidth = c(0, 700, 2000, 5000)))
# com data.table =  4.093 segundos
# sem data.table = 36.683 segundos
# com matrizes   =  0.249 segundos

# load sample data from package segregr
marilia_sf <- st_read(system.file("extdata/marilia_2010.gpkg", package = "segregr"))


data <- marilia_sf
id_field = "id"
distance_method = "geodist"
bandwidths <- 1000
bandwidths <- c(0, 500, 1000)

# calculate segregation metrics
system.time(segregation <- measure_segregation(marilia_sf, bandwidths = c(0, 500, 750, 1000)))
segregation_results <- segregation

global_metrics_to_df(segregation) %>% View()
local_metrics_to_sf(segregation) %>% View()

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

isolation_to_sf(segregation) %>% filter(group == "Wbritish") %>%
  ggplot() +
  geom_sf(aes(fill = isolation), colour = NA) +
  scale_fill_viridis() +
  facet_wrap(~bw) +
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

segregation$Q %>% filter(group == "Wbritish")

# smooth
# 1:    0 Wbritish 0.5583243
# 2:  700 Wbritish 0.5433872
# 3: 2000 Wbritish 0.5076631
# 4: 5000 Wbritish 0.4510788

# original
# bw    group isolation
# 1:    0 Wbritish 0.5583243
# 2:  700 Wbritish 0.5490044
# 3: 2000 Wbritish 0.5227342
# 4: 5000 Wbritish 0.4785019



b <- 2000
d <- 1:10000
weight = exp((-0.5) * (d / b) * (d / b))
data <- tibble::tibble(distance = d, weight = weight)
ggplot(data %>% filter(weight >= 0.01)) + geom_point(aes(x=distance, y= weight)) + geom_vline(xintercept = b)


segregation_results$population %>%
  summarise_if(is.numeric, sum)

segregation_results$intensity %>%
  group_by(bw) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(across(.fns=sum))


