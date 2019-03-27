context("nhd_plus_query")

test_that("nhd_plus_query handles vpu boundaries well", {
  skip("This test is very long-running. Probably only suitable for manual execution.")
  skip_on_cran()
  skip_on_travis()

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
           function(x) length(x) > 0),], UnitType == "VPU")), 2)

})

test_that("nhd_plus_query fails well", {
  skip_on_cran()
  skip_on_travis()

  coords <- data.frame(lat = c(20.79722, 42.96523),
                       lon = c(-156.47833, -89.2527))

  expect_error(nhdR::nhd_plus_query(lat = coords$lat,
                                lon = coords$lon,
                                dsn = "NHDFlowLine",
                                buffer_dist = 0.1),
               "nhd_plus_query only accepts a single lon-lat pair.")
})


test_that("nhd_plus_query handles mismatched column names.", {
  skip_on_cran()
  skip_on_travis()

  # https://github.com/jsta/nhdR/issues/57
  box <- st_bbox(c(xmin = -105.93003, xmax = -104.91784,
                   ymin = 40.41176, ymax = 41.21014),
                crs = st_crs(4326)) %>%
    st_as_sfc()

  testthat::expect_success(
  poudre_flow <- nhd_plus_query(poly = box,
                                dsn = c('NHDFlowLine'))
  )

})
