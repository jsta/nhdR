#' List available locally cached NHD layers per state
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

#' List available locally cached NHDplus layers per state
#'
#' @export
#' @param vpu numeric vector processing unit
#' @param component character component name
#' @param file_ext character choice of "shp" for spatial data and
#' "dbf" for non-spatial. optional
#' @param ... arguments passed to list.files. optional.
#' @importFrom rgdal ogrListLayers
#'
#' @examples \dontrun{
#' nhd_plus_list(vpu = 4)
#' nhd_plus_list(vpu = 4, full.names = TRUE)
#'
#' nhd_plus_list(vpu = 1, component = "NHDPlusAttributes")
#' nhd_plus_list(vpu = "National", component = "V1_To_V2_Crosswalk")
#'
#' }
nhd_plus_list <- function(vpu, component = "NHDSnapshot", file_ext = NA, ...){

  candidate_dirs <- list.dirs(file.path(nhd_path(), "NHDPlus"),
                              full.names = TRUE, recursive = FALSE)
  if(vpu == "National"){
    target_dir <- candidate_dirs[grep(vpu, candidate_dirs)]
  }else{
    target_dir <- candidate_dirs[grep(
                    paste0(zero_pad(vpu, 1)), candidate_dirs)]
    target_dir <- target_dir[grep(component, target_dir)]
  }

  res <- list.files(target_dir, pattern = "dbf|DBF|shp", ...)

  if(length(grep(file_ext, res)) == 0 | is.na(file_ext)){
    res
  }else{
    res[grep(paste0(file_ext, "$"), res)]
  }
}

