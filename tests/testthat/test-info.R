context("info")

test_that("info functions work", {
  skip_on_cran()
  skip_on_ci()

  res <- nhd_plus_info(vpu = 4, component = "NHDSnapshot", dsn = "NHDWaterbody")
  expect_equal(length(unique(as.data.frame(res)$Var2)),
                      13)

  nhd_get("DC")
  res <- nhd_info("DC", "NHDWaterbody")
  expect_equal(length(unique(as.data.frame(res)$Var2)),
                      14)

  # works with dbf layers
  nhd_plus_get(1, "NHDPlusAttributes")
  expect_equal(
    class(nhd_plus_info(vpu = 1, component = "NHDPlusAttributes", dsn = "PlusFlow")),
    "table")
})
