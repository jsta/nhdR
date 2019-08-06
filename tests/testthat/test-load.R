context("load")

test_that("nhd_plus_load works with non-numeric VPU characters", {
  skip_on_cran()
  skip_on_travis()

  y = nhd_plus_load(vpu='03N', component='NHDPlusAttributes',
                    dsn='PlusFlowlineVAA', approve_all_dl=TRUE)
  x = nhd_plus_load(vpu='03S', component='NHDPlusAttributes',
                    dsn='PlusFlowlineVAA', approve_all_dl=TRUE)

  expect_false(identical(x, y))
})
