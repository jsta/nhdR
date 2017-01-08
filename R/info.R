#' nhd_plus_info
#'
#' @param layer_name character
#' @export
#' @importFrom rgdal ogrInfo
#'
#' @examples \dontrun{
#' nhd_info("Wall")
#' }
nhd_plus_info <- function(layer_name){
  rgdal::ogrInfo(gdb_plus_path(), layer_name)
}



