context("load")

test_that("nhd_plus_load works for single digit VPU entries", {
  skip_on_cran()
  skip_on_ci()

  x <- nhd_plus_load(vpu = '8', component = "NHDPlusAttributes",
                         dsn = "PlusFlowLineVAA", approve_all_dl = TRUE)

  expect_s3_class(x, "data.frame")
})


test_that("nhd_plus_load handles non-numeric VPU characters", {
  skip_on_cran()
  skip_on_ci()

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


test_that("nhd_plus_load works with the wkt_filter argument", {
  skip_on_cran()
  skip_on_ci()

  expect_s3_class(
    nhd_plus_load(4, "NHDSnapshot", "NHDWaterbody", wkt_filter = "POINT (-85.411 42.399)"),
    "data.frame")
})

test_that("nhd_load works", {
  skip_on_cran()
  skip_on_ci()

  expect_s3_class(
    nhd_load(c("RI", "DC"), "NHDWaterbody"),
    "data.frame")
})


test_that("nhd_load(ing) dbf files works", {
  skip_on_cran()
  skip_on_ci()

  expect_s3_class(
    nhd_load("RI", "NHDReachCrossReference"),
    "data.frame"
    )
})
