context("info")

test_that("info functions work", {
  skip_on_cran()
  skip_on_travis()

  res <- nhd_plus_info(vpu = 4, component = "NHDSnapshot", dsn = "NHDWaterbody")
  expect_equal(length(res$iteminfo$name),
                      12)

  res <- nhd_info("DC", "NHDWaterbody")
  expect_equal(length(res$iteminfo$name),
                      13)
})
