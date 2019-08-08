context("load")

test_that("nhd_plus_load works for single digit VPU entries", {
  skip_on_cran()
  skip_on_travis()

  x <- nhd_plus_load(vpu = '8', component = "NHDPlusAttributes",
                         dsn = "PlusFlowLineVAA", approve_all_dl = TRUE)

  expect_s3_class(x, "data.frame")
})


test_that("nhd_plus_load handles non-numeric VPU characters", {
  skip_on_cran()
  skip_on_travis()

  x = nhd_plus_load(vpu='03S', component='NHDPlusAttributes',
                    dsn='PlusFlowlineVAA', approve_all_dl=TRUE)
  y = nhd_plus_load(vpu='03N', component='NHDPlusAttributes',
                    dsn='PlusFlowlineVAA', approve_all_dl=TRUE)

  expect_false(identical(x, y))

  z <- nhd_plus_load(vpu = '3N', component = "NHDPlusAttributes",
                         dsn = "PlusFlowLineVAA", approve_all_dl = TRUE)

  expect_true(identical(y, z))

  expect_error(
    nhd_plus_load(vpu = '03', component = "NHDPlusAttributes",
                  dsn = "PlusFlowLineVAA", approve_all_dl = TRUE),
    "03 is not a valid vpu. Are you missing a letter designation? See VPU map.",
    fixed = TRUE)
})
