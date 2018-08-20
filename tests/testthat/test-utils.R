context("utils")

test_that("long2UTM works", {
  expect_equal(long2UTM(-85), 16)
})

test_that("find_vpu works with character vpus", {
  pnt <- sf::st_sfc(sf::st_point(c(-80.8, 26.93)))
  sf::st_crs(pnt) <- 4326
  expect_equal(nhdR:::find_vpu(pnt), "03S")
})

test_that("find_vpu works with numeric vpus", {
  pnt <- sf::st_sfc(sf::st_point(c(-70.56, 43.85)))
  sf::st_crs(pnt) <- 4326
  expect_equal(nhdR:::find_vpu(pnt), "01")
})

test_that("toUTM works with objects spanning multiple utm zones", {
  data(gull)
  data(mendota)

  big_combined <- rbind(
    gull$sp$NHDWaterbody,
    mendota$sp$NHDWaterbody)

  expect_equal(sf::st_crs(toUTM(big_combined)),
               sf::st_crs(mendota$sp$NHDWaterbody))
  expect_equal(sf::st_crs(toUTM(big_combined)),
               sf::st_crs(gull$sp$NHDWaterbody))
})
