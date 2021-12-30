context("nhd_plus_query")

test_that("nhd_plus_query handles vpu boundaries well", {
  skip("This test is very long-running. Probably only suitable for manual execution.")
  skip_on_cran()
  skip_on_ci()

  coords <- data.frame(nhd_long = -89.21884, nhd_lat = 46.2052)
  lines  <- nhdR::nhd_plus_query(lat = coords$nhd_lat,
    lon = coords$nhd_long,
    dsn = "NHDFlowLine",
    buffer_dist = 0.1,
    quiet = TRUE)$sp$NHDFlowLine

  # expect lines intersect multiple vpus
  expect_equal(nrow(dplyr::filter(vpu_shp[
    sapply(st_intersects(vpu_shp,
      st_transform(lines, st_crs(vpu_shp))),
    function(x) length(x) > 0), ], UnitType == "VPU")), 2)

})

test_that("nhd_plus_query fails well", {
  skip_on_cran()
  skip_on_ci()

  coords <- data.frame(lat = c(20.79722, 42.96523),
    lon = c(-156.47833, -89.2527))
  expect_error(
    nhdR::nhd_plus_query(lat = coords$lat,
      lon = coords$lon,
      dsn = "NHDFlowLine"),
    "nhd_plus_query only accepts a single lon-lat pair.")

  expect_error(
    nhdR::nhd_plus_query(dsn = "NHDWaterbody", wkt_filter = "POINT (-85.411 42.399)"),
    "Must specify either lon and lat or poly but not both.")


  # query will produce a zero row sf object when appropriate
  coords <- data.frame(lat = c(42.04133),
    lon = c(-71.70511))
  poly <- nhd_plus_query(coords$lon, coords$lat, dsn = "NHDWaterbody",
    buffer_dist = units::as_units(1, "km"))
  expect_equal(nrow(poly$sp$NHDWaterbody), 0)
})


test_that("nhd_plus_query handles mismatched column names.", {
  skip_on_cran()
  skip_on_ci()

  # https://github.com/jsta/nhdR/issues/57
  box <- sf::st_bbox(c(xmin = -105.93003, xmax = -104.91784,
    ymin = 40.41176, ymax = 41.21014),
  crs = st_crs(4326)) %>%
    sf::st_as_sfc()
  poudre_flow <- nhd_plus_query(poly = box,
    dsn = c("NHDFlowLine"),
    quiet = TRUE,
    approve_all_dl = TRUE)

  expect_s3_class(poudre_flow$sp$NHDFlowLine, "sf")
})

test_that("nhd_query works.", {
  skip("This test is very long-running. Probably only suitable for manual execution.")
  skip_on_cran()
  skip_on_ci()

  wk <- wikilake::lake_wiki("Worden Pond")
  qry <- nhd_query(wk$Lon, wk$Lat, dsn = c("NHDWaterbody", "NHDFlowline"))
  testthat::expect_s3_class(qry$sp$NHDWaterbody, "data.frame")
})

test_that("nhd_query works with polygon inputs", {
  skip_on_cran()
  skip_on_ci()

  bbox <- sf::st_bbox(
    c(xmin = 283389.1, ymin = 4589520.3, xmax = 286345.2, ymax = 4591955.5),
    crs = "+proj=utm +zone=19 +datum=WGS84")
  qry <- nhd_query(poly = sf::st_as_sfc(bbox), dsn = c("NHDWaterbody", "NHDFlowLine"))
  # mapview(bbox) + mapview(qry$sp$NHDWaterbody) + mapview(qry$sp$NHDFlowLine)

  testthat::expect_lt(nrow(qry$sp$NHDWaterbody), 20)
  testthat::expect_s3_class(qry$sp$NHDWaterbody, "data.frame")
  testthat::expect_lt(nrow(qry$sp$NHDFlowLine), 20)
})