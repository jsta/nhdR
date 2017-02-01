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
#' @param fpath file.path to nhd plus gdb file. optional.
#' @importFrom rgdal ogrListLayers
#'
#' @examples \dontrun{
#' nhd_plus_list()
#' nhd_plus_list(fpath = nhdR::gdb_plus_path())
#' }
nhd_plus_list <- function(fpath = NA){
  if(!is.na(fpath)){
    rgdal::ogrListLayers(fpath)
  }else{
    rgdal::ogrListLayers(gdb_plus_path())
  }
}

