# nocov start
utils::globalVariables(c(".", "%>%", ":=", "%like%", "%chin%", "set"))

.onLoad <- function(lib, pkg) {
  requireNamespace("sf")
  requireNamespace("dplyr")
  requireNamespace("data.table")
  requireNamespace("geodist")
  requireNamespace("tmap")
}

#' @importFrom data.table := %between% fifelse %chin% set
NULL

#' @importFrom tmap tm_shape tm_polygons tm_layout
NULL

utils::globalVariables(c(".", "%>%", ":="))

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(
  c('id', 'group', 'bw', 'd', 'filter', 'e',
    'isolation', 'exposure', 'group_b', 'group_a', 'metric', 'value',
    'h', 'population', 'distances', 'dm', 'group_entropy group_proportion_city',
    'group_proportion_locality', 'groups', 'i.group_proportion_city', 'i.intensity',
    'i.population', 'i.proportion_group_city', 'i.proportion_group_locality',
    'intensity', 'inv_proportion', 'isolation_exposure', 'k', 'partial_I',
    'population.from', 'population.to', 'population_group_city',
    'population_intensity_locality', 'population_locality', 'proportion',
    'proportion_abs_diff', 'proportion_group_city', 'proportion_group_city_a',
    'proportion_group_locality', 'proportion_group_locality_b',
    'group_entropy', 'group_proportion_city', 'total_population'))

# nocov end

