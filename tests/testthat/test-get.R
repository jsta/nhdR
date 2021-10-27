context("get")

library(nhdR)

test_that("temporary flag works", {
  skip_on_cran()
  skip_on_ci()

  # nhdR_path_original <- Sys.getenv("nhdR_path")
  # Sys.setenv(nhdR_path = "")  
  # nhdR::nhd_path(TRUE) # temp
  # Sys.setenv(nhdR_path = "")
  # nhdR::nhd_path(FALSE) # appdirs

  cleanup_existing <- function(path){
    zfile <- paste0(path, "/NHDPlus/NHDPlusV21_HI_20_NHDSnapshot_02.7z")
    path <- paste0(path, "/NHDPlus/HI_20_NHDSnapshot")    
    # print(zfile)
    if (length(dir(path)) > 0){    
      unlink(path, recursive = TRUE)
      print("files removed")
    }
    if (file.exists(zfile)){    
      unlink(zfile)
      print("7z file removed")
    }
  }
  nhdR_path_original <- Sys.getenv("nhdR_path")
  cleanup_existing(nhdR:::nhd_path())
  cleanup_existing(nhdR_path_original)  
 
  ## enable temporary  
  Sys.setenv(nhdR_path = "")
  cleanup_existing(nhdR:::nhd_path())
  # print(Sys.getenv("nhdR_path"))
  # nhdR::nhd_path()

  ## download to temp
  nhd_plus_get(20, temporary=TRUE)
  path <- paste0(nhdR:::nhd_path(), "/NHDPlus/HI_20_NHDSnapshot")
  zfile <- paste0(nhdR:::nhd_path(), "/NHDPlus/NHDPlusV21_HI_20_NHDSnapshot_02.7z")  
  expect_true(file.exists(zfile))
  expect_gt(length(dir(path)), 0)

  ## verify nothing in rappdirs  
  path <- paste0(nhdR_path_original, "/NHDPlus/HI_20_NHDSnapshot")
  zfile <- paste0(nhdR_path_original, "/NHDPlus/NHDPlusV21_HI_20_NHDSnapshot_02.7z")
  expect_false(file.exists(zfile))
  expect_equal(length(dir(path)), 0)

  ## download to appdirs  
  Sys.setenv(nhdR_path = "")
  nhd_plus_get(20, temporary=FALSE)
  path <- paste0(nhdR_path_original, "/NHDPlus/HI_20_NHDSnapshot")
  zfile <- paste0(nhdR_path_original, "/NHDPlus/NHDPlusV21_HI_20_NHDSnapshot_02.7z")
  expect_true(file.exists(zfile))
  expect_gt(length(dir(path)), 0)

})

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
  skip_on_ci()

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
