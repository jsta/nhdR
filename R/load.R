#' nhd_load
#'
#' @param state character state abbreviation
#' @param layer_name character name of a NHD layer
#' @param ... arguments passed to sf::st_read
#'
#' @return spatial object
#' @importFrom sf st_read
#' @export
#'
#' @examples \dontrun{
#' dt <- nhd_load(c("CT", "RI"), "NHDWaterbody")
#' dt <- nhd_load(c("CT", "RI"), "NHDWaterbody", quiet = TRUE)
#' }
nhd_load <- function(state, layer_name, ...){
  nhd_load_state <- function(state, ...){
    if(any(!file.exists(gdb_path(state)))){
      nhd_get(state = state)
    }
      sf::st_read(gdb_path(state), layer_name, ...)
  }

  invisible(prj <- sf::st_crs(nhd_load_state(state[1], quiet = TRUE)))

  res <- do.call("rbind", lapply(state, nhd_load_state, ...))

  sf::st_crs(res) <- prj
  res
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
#' dt <- nhd_plus_load(c(1,2), "NHDWaterbody")
#' dt <- nhd_plus_load(4, "NHDFlowline")
#' }
nhd_plus_load <- function(vpu, component){

  nhd_plus_load_vpu <- function(vpu, ...){
    vpu_path <- file.path(nhd_path(), "NHDPlus",
                          basename(get_plus_remotepath(vpu)))
    if(any(!file.exists(vpu_path))){
      nhd_plus_get(vpu = vpu)
    }

    candidate_files <- nhd_plus_list(vpu, full.names = TRUE)
    res <- candidate_files[grep(tolower(component), tolower(candidate_files))]
    sf::st_zm(sf::st_read(res, ...))
  }

  invisible(prj <- sf::st_crs(nhd_plus_load_vpu(vpu[1], quiet = TRUE)))
  res <- do.call("rbind", lapply(vpu, nhd_plus_load_vpu))
  sf::st_crs(res) <- prj
  res
}
