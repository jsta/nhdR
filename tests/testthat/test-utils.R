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
