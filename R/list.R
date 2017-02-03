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
#' @param vpu file.path to nhd plus gdb file. optional.
#' @param component character component name
#' @importFrom rgdal ogrListLayers
#'
#' @examples \dontrun{
#' nhd_plus_list(vpu = 4)
#' }
nhd_plus_list <- function(vpu, component = NA){

  candidate_dirs <- list.dirs(file.path(nhd_path(), "NHDPlus"),
                              full.names = TRUE, recursive = FALSE)
  target_dir <- candidate_dirs[grep(
                    paste0(zero_pad(vpu, 1), "|", component), candidate_dirs)]

  list.files(target_dir, pattern = "shp$")
}

