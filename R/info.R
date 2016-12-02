#' nhd_info
#'
#' @param layer_name character
#' @export
#' @importFrom rgdal ogrInfo
#'
#' @examples \dontrun{
#' nhd_info("Wall")
#' }
nhd_info <- function(layer_name){
  rgdal::ogrInfo(gdb_path(), layer_name)
}



