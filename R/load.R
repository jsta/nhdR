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
#' @param dsn data source name
#' @return spatial object
#' @importFrom sf st_read st_zm
#' @importFrom foreign read.dbf
#' @export
#'
#' @examples \dontrun{
#' # Spatial
#' dt <- nhd_plus_load(4, "NHDSnapshot", "NHDWaterbody")
#' dt <- nhd_plus_load(c(1,2), "NHDSnapshot", "NHDWaterbody")
#' dt <- nhd_plus_load(4, "NHDSnapshot", "NHDFlowline")
#'
#' # Non-spatial
#' dt <- nhd_plus_load(1, "NHDPlusAttributes", "PlusFlow")
#' }
nhd_plus_load <- function(vpu, component, dsn){

  nhd_plus_load_vpu <- function(vpu, component, dsn, ...){
    vpu_path <- file.path(nhd_path(), "NHDPlus",
                          basename(get_plus_remotepath(vpu, component)))

    if(any(!file.exists(vpu_path))){
      nhd_plus_get(vpu = vpu, component = component)
    }

    candidate_files <- nhd_plus_list(vpu, component = component,
                                     full.names = TRUE)
    res <- candidate_files[grep(paste0(tolower(dsn), "\\."),
                                tolower(candidate_files))]

    if(length(grep("shp$", res)) > 0){
      res <- sf::st_read(res, ...)
      is_spatial <- TRUE
      list(res = res, is_spatial = is_spatial)
    }else{
      res <- lapply(res, foreign::read.dbf)
      names(res) <- dsn
      is_spatial <- FALSE
      list(res = res[[dsn]], is_spatial = is_spatial)
    }
  }

  res <- lapply(vpu, nhd_plus_load_vpu, component = component, dsn = dsn)
  is_spatial <- unlist(lapply(res, function(x) x$is_spatial))
  res <- do.call("rbind", lapply(res, function(x) x$res))

  if(any(is_spatial)){
    invisible(prj <- sf::st_crs(nhd_plus_load_vpu(vpu[1],
                      component = component, dsn = dsn, quiet = TRUE)))
    sf::st_crs(res) <- prj
  }

  res
}
