


marilia_sf <- st_read(here::here("inst/extdata", "marilia_2010.gpkg")) %>%
  rename(geometry = geom)

segregation <- measure_segregation(marilia_sf)

segregation$d

segregation_results <- segregation




