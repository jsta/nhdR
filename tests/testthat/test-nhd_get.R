context("nhd_get")

test_that("nhd_get fails well", {
  expect_error(nhd_get(state = "gibberish"),
               "gibberish is not a valid state abbreviation")
})

test_that("nhd_plus_get fails well", {
  expect_error(nhd_plus_get(vpu = 100),
               "100 is not a valid vpu")
})
