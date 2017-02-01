#' nhd_info
#'
#' @param state character
#' @param layer_name character
#' @export
#' @importFrom rgdal ogrInfo
#'
#' @examples \dontrun{
#' nhd_info("DC", "NHDWaterbody")
#' }
nhd_info <- function(state, layer_name){
  rgdal::ogrInfo(gdb_path(state), layer_name)
}

#' nhd_plus_info
#'
#' @param layer_name character
#' @param fpath file.path to nhd plus gdb file. optional.
#' @export
#' @importFrom rgdal ogrInfo
#'
#' @examples \dontrun{
#' nhd_plus_info("Wall")
#' nhd_plus_info("Wall", fpath = nhdR::gdb_plus_path())
#' }
nhd_plus_info <- function(layer_name, fpath = NA){
  if(!is.na(fpath)){
    rgdal::ogrInfo(fpath, layer_name)
  }else{
    rgdal::ogrInfo(gdb_plus_path(), layer_name)
  }
}



