#' nhd_load
#'
#' @param state character state abbreviation
#' @param layer_name character name of a NHD layer
#'
#' @return spatial object
#' @importFrom sf st_read
#' @export
#'
#' @examples \dontrun{
#' dt <- nhd_load(c("CT", "RI"), "NHDWaterbody")
#' }
nhd_load <- function(state, layer_name){
  nhd_load_state <- function(state){
    if(any(!file.exists(gdb_path(state)))){
      nhd_get(state = state)
      # stop(paste0(gdb_path(state), " not found. Try nhd_get()"))
    }

      sf::st_read(gdb_path(state), layer_name)
  }

  do.call("rbind", lapply(state, nhd_load_state))
}

#' nhd_plus_load
#'
#' @param layer_name character name of a NHD layer
#'
#' @return spatial object
#' @importFrom sf st_read
#' @export
#'
#' @examples \dontrun{
#' dt <- nhd_plus_load("NHDWaterbody")
#' }
nhd_plus_load <- function(layer_name){
  sf::st_read(gdb_plus_path(), layer_name)
}
