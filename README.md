
<!-- README.md is generated from README.Rmd. Please edit that file -->

# segregr

<!-- badges: start -->
<!-- badges: end -->

The goal of segregr is to provide and easy way of calculating spatial
segregation metrics in R.

## Installation

You can install the development version of segregr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mvpsaraiva/segregr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(sf)
#> Linking to GEOS 3.8.1, GDAL 3.1.1, PROJ 6.3.1
library(segregr)

# load sample data from package segregr
marilia_sf <- st_read(system.file("extdata/marilia_2010.gpkg", package = "segregr"))
#> Reading layer `marilia_2010_proj' from data source `/private/var/folders/p_/v724hrfn46v1sxqm4xtxfv580000gn/T/RtmpvtT7ph/temp_libpath56fe6c416f9c/segregr/extdata/marilia_2010.gpkg' using driver `GPKG'
#> Simple feature collection with 295 features and 10 fields
#> geometry type:  POLYGON
#> dimension:      XY
#> bbox:           xmin: 597725.2 ymin: 7535553 xmax: 612714.8 ymax: 7551388
#> projected CRS:  SIRGAS 2000 / UTM zone 22S

# calculate segregation metrics
segregation <- measure_segregation(marilia_sf)
```

``` r
# global dissimilarity index
segregation$D
#> [1] 0.2565095

#' # global entropy
segregation$E
#> [1] 1.733732

# global information theory index H
segregation$H
#> [1] 0.110396
```
