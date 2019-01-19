context("extract_network")

test_that("extract_network fails well if passed multiple coordinates", {
  skip_on_cran()
  skip_on_travis()

  coords <- data.frame(lat = c(20.79722, 42.96523),
                       lon = c(-156.47833, -89.2527))
  expect_error(extract_network(coords$lon, coords$lat, maxsteps = 9),
               "extract_network only accepts a single lon-lat pair.")
})
