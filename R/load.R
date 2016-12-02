#' nhd_load
#'
#' @param layer_name character name of a NHD layer
#'
#' @return spatial object
#' @importFrom sf st_read
#' @export
#'
#' @examples \dontrun{
#' nhd_load("NHDWaterbody")
#' }
nhd_load <- function(layer_name){
  sf::st_read(gdb_path(), layer_name)
}
