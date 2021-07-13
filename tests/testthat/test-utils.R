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

test_that("toUTM works", {
  skip_on_cran()
  skip_on_ci()

  data(gull)
  data(mendota)
  data(sunapee)

  # with non-longlat objects
  expect_equal(sf::st_crs(toUTM(mendota$sp$NHDWaterbody)),
    sf::st_crs(mendota$sp$NHDWaterbody))

  #  with objects spanning multiple utm zones
  big_combined <- rbind(
    st_transform(gull$sp$NHDWaterbody, 4326),
    st_transform(sunapee$sp$NHDWaterbody, 4326))

  expect_true(get_utm_zone(st_crs(toUTM(gull$sp$NHDWaterbody))) ==
    "16")

  big_combined <- rbind(
    st_transform(sunapee$sp$NHDWaterbody, 4326),
    st_transform(gull$sp$NHDWaterbody, 4326))

  expect_true(st_crs(toUTM(sunapee$sp$NHDWaterbody)) ==
    st_crs(toUTM(big_combined)))

})