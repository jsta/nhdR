context("nhd_plus_query")

test_that("nhd_plus_query handles vpu boundaries well", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  coords           <- data.frame(nhd_long = -89.21884, nhd_lat = 46.2052)
  point <- sf::st_sfc(sf::st_point(c(coords$nhd_long, coords$nhd_lat)), crs = 4326)
  lines <- nhdR::nhd_plus_query(lat = coords$nhd_lat,
                                           lon = coords$nhd_long,
                                           dsn = "NHDFlowLine",
                                           buffer_dist = 0.1)$sp$NHDFlowLine

  # expect lines intersect multiple vpus
  expect_equal(nrow(dplyr::filter(vpu_shp[
    sapply(st_intersects(vpu_shp,
                         st_transform(lines, st_crs(vpu_shp))),
           function(x) length(x) > 0),], UnitType == "VPU")), 2)

})
