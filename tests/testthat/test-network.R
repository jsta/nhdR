context("network")

test_that("extract_network fails well if passed multiple coordinates", {
  skip_on_cran()
  skip_on_ci()

  coords <- data.frame(lat = c(20.79722, 42.96523),
                       lon = c(-156.47833, -89.2527))
  expect_error(
    extract_network(coords$lon, coords$lat, maxsteps = 9),
               "extract_network only accepts a single lon-lat pair.")
})

test_that("extract_network is silent if passed projected buffer extents", {
  skip_on_cran()
  skip_on_ci()

  coords <- data.frame(lat = c(20.79722, 42.96523),
                       lon = c(-156.47833, -89.2527))
  expect_silent(
    res <- extract_network(coords$lon[1], coords$lat[1], maxsteps = 9,
                  buffer_dist = units::as_units(5, "km"), quiet = TRUE)
    )
})

test_that("terminal_reaches fails well", {
  skip_on_cran()
  skip_on_ci()

  #  if point intersects Great Lakes
  coords  <- data.frame(lat = 44.6265, lon = -86.23121)

  expect_error(
    terminal_reaches(coords$lon, coords$lat,
                     buffer_dist = units::as_units(0.5, "km"), quiet = TRUE),
    "This point intersects one of the Great Lakes. NHD doesn't support finding their terminal reach.")

  # if intersecting lake is isolated
  coords  <- data.frame(lat = 44.48647, lon = -86.07475)

  expect_error(
    terminal_reaches(coords$lon, coords$lat,
                     buffer_dist = units::as_units(0.5, "km"), quiet = TRUE),
    "No streams intersect this lake polygon")

  # if point doesn't intersect a waterbody
  coords <- data.frame(lat = c(42.04133),
                       lon = c(-71.70511))

  expect_error(
    terminal_reaches(coords$lon, coords$lat,
                     buffer_dist = units::as_units(1, "km"), quiet = TRUE),
    "No lake polygon found at query point")

})

test_that("leaf_reaches works", {
  skip_on_cran()
  skip_on_ci()

  coords <- data.frame(lat = 20.79722, lon = -156.47833)
  network <- nhd_plus_query(lon = coords$lon, lat = coords$lat,
                             dsn = "NHDFlowline",
                            buffer_dist = units::as_units(2, "km"),
                            quiet = TRUE)
  network <- network$sp$NHDFlowline
  expect_s3_class(leaf_reaches(network = network), "sf")

  expect_s3_class(leaf_reaches(coords$lon, coords$lat, quiet = TRUE), "sf")
})


