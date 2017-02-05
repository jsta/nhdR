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
#' @param vpu numeric vector processing unit
#' @param component character component name
#' @return spatial object
#' @importFrom sf st_read st_zm
#' @export
#'
#' @examples \dontrun{
#' dt <- nhd_plus_load(4, "NHDWaterbody")
#' }
nhd_plus_load <- function(vpu, component = NA){
  candidate_files <- nhd_plus_list(vpu, full.names = TRUE)
  res <- candidate_files[grep(component, candidate_files)]
  sf::st_zm(sf::st_read(res))
}
