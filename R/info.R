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
#' @param vpu numeric vector processing unit
#' @param component character component name
#' @export
#' @importFrom rgdal ogrInfo
#'
#' @examples \dontrun{
#' nhd_plus_info(vpu = 4, component = "NHDWaterbody")
#' }
nhd_plus_info <- function(vpu, component = NA){
  candidate_files <- nhd_plus_list(vpu, full.names = TRUE)
  res <- candidate_files[grep(component, candidate_files)]
  rgdal::ogrInfo(res, component)
}



