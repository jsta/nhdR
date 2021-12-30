context("list")

test_that("list functions work", {
  skip_on_cran()
  skip_on_ci()

  short_list <- nhd_plus_list(vpu = 4)
  long_list  <- nhd_plus_list(vpu = 4, full.names = TRUE)

  expect_true(all(nchar(short_list) < nchar(long_list)))

  expect_equal(class(nhd_plus_list(vpu = 4, component = "NHDPlusAttributes")),
    "character")

  nhd_plus_get(vpu = "National", component = "V1_To_V2_Crosswalk")
  expect_equal(length(nhd_plus_list(vpu = "National",
    component = "V1_To_V2_Crosswalk")),
  1)

  expect_true(length(nhd_plus_list(vpu = 4, file_ext = "shp")) <
    length(short_list))

  nhd_get("DC")
  expect_equal(length(nhd_list("DC")), 29)
})