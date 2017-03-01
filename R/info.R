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
#' @param dsn character data source name
#' @export
#' @importFrom rgdal ogrInfo
#'
#' @examples \dontrun{
#' nhd_plus_info(vpu = 4, component = "NHDSnapshot", dsn = "NHDWaterbody")
#' nhd_plus_info(vpu = 1, component = "NHDPlusAttributes", dsn = "PlusFlow")
#' }
nhd_plus_info <- function(vpu, component, dsn){
  candidate_files <- nhd_plus_list(vpu, component = component, full.names = TRUE)
  res <- candidate_files[grep(component, candidate_files)]
  res <- res[grep(dsn, res)]

  if(is_spatial(res)){
    rgdal::ogrInfo(res, dsn)
  }else{
    summary(foreign::read.dbf(res))
  }
}



