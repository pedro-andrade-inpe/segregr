library(tidyverse)
library(sf)
library(segregr)
library(scales)


# Marília -----------------------------------------------------------------

marilia_sf <- st_read(system.file("extdata/marilia_2010.gpkg", package = "segregr"))
marilia_even <-  marilia_sf %>%
    mutate_at(vars(starts_with("mw")), quantile, probs = 0.75) %>%
    mutate_at(vars(starts_with("mw")), round)

marilia_random <-  marilia_sf %>%
  mutate_at(vars(starts_with("mw")), runif, min=0, max=100 %>%
  mutate_at(vars(starts_with("mw")), round)


marilia_segreg <- measure_segregation(marilia_sf, bandwidths = seq(100, 15000, 500))
marilia_unsegreg <- measure_segregation(marilia_even, bandwidths = seq(100, 15000, 500))
marilia_rnsegreg <- measure_segregation(marilia_random, bandwidths = seq(100, 15000, 500))

rbind(
  marilia_segreg$D %>% mutate(scenario = "real"),
  marilia_unsegreg$D %>% mutate(scenario = "even"),
  marilia_rnsegreg$D %>% mutate(scenario = "random")
) %>%
  # filter(scenario == "even") %>%
  ggplot(aes(x=bw, y = D, color = scenario)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = comma_format(accuracy = 0.0000001))

rbind(
  marilia_segreg$H %>% mutate(scenario = "real"),
  marilia_unsegreg$H %>% mutate(scenario = "even"),
  marilia_rnsegreg$H %>% mutate(scenario = "random")
) %>%
  # filter(scenario == "even") %>%
  ggplot(aes(x=bw, y = H, color = scenario)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = comma_format(accuracy = 0.0000001))


# GLA ---------------------------------------------------------------------

gla_sf <- st_read(system.file("extdata/gla.gpkg", package = "segregr"))
gla_even <-  gla_sf %>%
  mutate_at(vars(Wbritish:Black), quantile, probs = 0.75) %>%
  mutate_at(vars(Wbritish:Black), round)

gla_random <-  gla_sf %>%
  mutate_at(vars(Wbritish:Black), runif, min=0, max=100) %>%
  mutate_at(vars(Wbritish:Black), round)


gla_segreg <- measure_segregation(gla_sf, bandwidths = seq(100, 15000, 500))
gla_unsegreg <- measure_segregation(gla_even, bandwidths = seq(100, 15000, 500))
gla_rnsegreg <- measure_segregation(gla_random, bandwidths = seq(100, 15000, 500))

rbind(
  gla_segreg$D %>% mutate(scenario = "real"),
  gla_unsegreg$D %>% mutate(scenario = "even"),
  gla_rnsegreg$D %>% mutate(scenario = "random")
) %>%
  # filter(scenario == "even") %>%
  ggplot(aes(x=bw, y = D, color = scenario)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = comma_format(accuracy = 0.0000001))

rbind(
  gla_segreg$H %>% mutate(scenario = "real"),
  gla_unsegreg$H %>% mutate(scenario = "even"),
  gla_rnsegreg$H %>% mutate(scenario = "random")
) %>%
  # filter(scenario == "even") %>%
  ggplot(aes(x=bw, y = H, color = scenario)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = comma_format(accuracy = 0.0000001))







