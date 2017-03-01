#' nhd_list
#'
#' @export
#' @param state character state abbreviation
#' @importFrom rgdal ogrListLayers
#'
#' @examples \dontrun{
#' nhd_list(state = "DC")
#' }
nhd_list <- function(state){
  rgdal::ogrListLayers(gdb_path(state))
}

#' nhd_plus_list
#'
#' @export
#' @param vpu numeric vector processing unit
#' @param component character component name
#' @param ... arguments passed to list.files. optional.
#' @importFrom rgdal ogrListLayers
#'
#' @examples \dontrun{
#' nhd_plus_list(vpu = 4)
#' nhd_plus_list(vpu = 4, full.names = TRUE)
#'
#' nhd_plus_list(vpu = 1, component = "NHDPlusAttributes")
#' }
nhd_plus_list <- function(vpu, component = "NHDSnapshot", ...){

  candidate_dirs <- list.dirs(file.path(nhd_path(), "NHDPlus"),
                              full.names = TRUE, recursive = FALSE)
  target_dir <- candidate_dirs[grep(
                    paste0(zero_pad(vpu, 1)), candidate_dirs)]
  target_dir <- target_dir[grep(component, target_dir)]

  # list.files(target_dir, pattern = "shp$", ...)
  res <- list.files(target_dir, pattern = "dbf|shp", ...)
  if(length(grep("shp$", res)) > 0){
    res <- res[grep("shp$", res)]
  }

  res
}

