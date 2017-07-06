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
#' @param file_ext character choice of "shp" for spatial data and
#' "dbf" for non-spatial. optional
#' @export
#' @importFrom rgdal ogrInfo
#'
#' @examples \dontrun{
#' nhd_plus_info(vpu = 4, component = "NHDSnapshot", dsn = "NHDWaterbody")
#' nhd_plus_info(vpu = 1, component = "NHDPlusAttributes", dsn = "PlusFlow")
#' }
nhd_plus_info <- function(vpu, component, dsn, file_ext = NA){
  candidate_files <- nhd_plus_list(vpu, component = component,
                                   full.names = TRUE, file_ext = file_ext)
  res <- candidate_files[grep(paste0(tolower(dsn), "\\."),
                              tolower(candidate_files))]

  res <- res[grep(dsn, res)]

  if(length(grep(paste0("shp", "$"), res)) > 0){
    res <- res[grep("shp$", res)]
    rgdal::ogrInfo(res, dsn)
  }else{
    summary(foreign::read.dbf(res))
  }
}
