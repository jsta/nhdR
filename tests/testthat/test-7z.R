context("7z")

test_that("7z is available", {
  skip_on_cran()

  testthat::expect_true(nhdR:::has_7z()$yes)
})
