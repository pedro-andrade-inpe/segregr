test_that("adjust_speed", {
    marilia_sf <- sf::st_read(system.file("extdata/marilia_2010.gpkg", package = "segregr"))
    segregation <- measure_segregation(marilia_sf)
    result <- exposure_isolation_matrix(segregation)
    
    expect_equal(sum(result[,-1], na.rm = TRUE), 9, tolerance = 0.00001)

    expect_true(is.na(result[1, 2]))
})
