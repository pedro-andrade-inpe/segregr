library("tidyverse")
library("sf")


marilia_sf <- st_read(here::here("inst/extdata", "Marilia2010.shp"))

marilia_sf <- marilia_sf %>%
  select(id = Cod_setor, respmeiosm:respacima2)

marilia_sf

colnames(marilia_sf) <- c("id",
                          "mw_0_to_05", "mw_05_to_1", "mw_1_to_2",
                          "mw_2_to_3", "mw_3_to_5", "mw_5_to_10",
                          "mw_10_to_15", "mw_15_to_20", "mw_20_above",
                          "geometry")

marilia_sf <- st_transform(marilia_sf, crs = 4326)

st_write(marilia_sf, here::here("inst/extdata", "marilia_2010.gpkg"))

