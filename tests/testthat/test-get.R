context("get")

test_that("nhd_get fails well", {
  expect_error(nhd_get(state = "gibberish"),
               "gibberish is not a valid state abbreviation")
})

test_that("nhd_plus_get fails well", {
  expect_error(nhd_plus_get(vpu = 100),
               "100 is not a valid vpu")
})

#' @importFrom crul ok
#' @importFrom fauxpas http200

test_that("remote urls are constructed correctly", {
  skip_on_cran()
  skip_on_travis()

  expect_true(
    crul::ok(
      nhdR:::get_plus_remotepath(4, component = "NHDSnapshot")
      )
    )

  expect_true(
    crul::ok(
      nhdR:::get_plus_remotepath("National", component = "V1_To_V2_Crosswalk"),
      status = 200L)
    )


})

# DC is the smallest nhd regular state
# 20 is the smallest nhd plus vpu
