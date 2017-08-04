#' nhd_load
#'
#' @param state character state abbreviation
#' @param layer_name character name of a NHD layer
#' @param file_ext character choice of "shp" for spatial data and
#' "dbf" or "gpkg" for non-spatial. optional
#' @param ... arguments passed to sf::st_read
#'
#' @return spatial object
#' @importFrom sf st_read
#' @importFrom gdalUtils ogr2ogr
#' @importFrom rlang quo
#' @importFrom dplyr tbl select src_sqlite
#' @export
#'
#' @examples \dontrun{
#' dt <- nhd_load(c("RI"), c("NHDWaterbody"))
#' dt <- nhd_load(c("CT", "RI"), "NHDWaterbody")
#' dt <- nhd_load(c("CT", "RI"), "NHDWaterbody", quiet = TRUE)
#' dt <- nhd_load("MI", "NHDFlowline")
#' dt <- nhd_load("RI", "NHDReachCrossReference")
#' dt <- nhd_load("RI", "NHDWaterbody", file_ext = "dbf")
#' }
nhd_load <- function(state, layer_name, file_ext = NA, ...){

  if(!(file_ext %in% c(NA, "shp", "dbf", "gpkg"))){
    stop(paste0("file_ext must be set to either 'shp' or 'dbf'"))
  }

  nhd_state_exists <- function(state){
    if(any(!file.exists(gdb_path(state)))){
      state_exists <- 0
      userconsents <- utils::menu(c("Yes", "No"),
                        title = paste0(state,
                        " state gdb file not found. Download it?"))
      if(userconsents == 1){
        yes_dl <- 1
      }else{
        yes_dl <- 0
      }
    }else{
      state_exists <- 1
      yes_dl <- 0
    }
    data.frame(state_exists = state_exists, yes_dl = yes_dl)
  }

  nhd_dl_state <- function(state, state_exists, yes_dl, file_ext, ...){

      if(as.logical(yes_dl)){
        nhd_get(state = state)
      }
      if(as.logical(state_exists) | as.logical(yes_dl)){

        if(is.na(file_ext) | file_ext == "shp"){
          tryCatch({
            sf::st_zm(sf::st_read(gdb_path(state), layer_name,
              stringsAsFactors = FALSE, ...))},
          error = function(e) {
            temp_dir <- tempdir()
            gdalUtils::ogr2ogr(gdb_path(state), temp_dir, layer_name)
            read.dbf(file.path(temp_dir, paste0(layer_name, ".dbf")))
          })
        }else{
          if(file_ext == "gpkg"){
            if(!is_gpkg_installed()){
              stop("The geopackage driver is not installed.")
            }
            gpkg_path <- gsub(".gdb", ".gpkg", gdb_path(state))
            if(!file.exists(gpkg_path)){
              compile_gpkg(state)
            }
            res <- dplyr::tbl(dplyr::src_sqlite(gpkg_path), layer_name)
            geom <- rlang::quo("geom")
            data.frame(dplyr::select(res, -geom))
          }else{
            temp_dir <- tempdir()
            gdalUtils::ogr2ogr(gdb_path(state), temp_dir, layer_name)
            read.dbf(file.path(temp_dir, paste0(layer_name, ".dbf")))
          }
        }
      }
  }

  first_state_exists <- nhd_state_exists(state[1])

  if(length(state) > 1){
    yes_dl_vec <- rbind(first_state_exists,
          do.call("rbind", lapply(state[2:length(state)],
              nhd_state_exists)))
  }else{
    yes_dl_vec <- first_state_exists
  }

  res <- lapply(seq_len(nrow(yes_dl_vec)),
                function(i) nhd_dl_state(state = state[i],
                                yes_dl = yes_dl_vec[i, "yes_dl"],
                                state_exists = yes_dl_vec[i, "state_exists"],
                                file_ext = file_ext,
                                ...))
  res <- res[!unlist(lapply(res, is.null))]
  res <- do.call("rbind", res)

  if(!all(class(res) != "data.frame") & any(class(res) == "sf")){
    invisible(prj <- sf::st_crs(nhd_dl_state(state = state[1],
                        state_exists = first_state_exists[,"state_exists"],
                        yes_dl = first_state_exists[,"yes_dl"],
                        quiet = TRUE, file_ext = NA)))
    sf::st_crs(res) <- prj
  }
  res
}

#' nhd_plus_load
#'
#' @param vpu numeric vector processing unit
#' @param component character component name
#' @param dsn data source name
#' @param file_ext character choice of "shp" for spatial data and
#' "dbf" for non-spatial. optional
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
#' dt <- nhd_plus_load("National", "V1_To_V2_Crosswalk",
#'  "NHDPlusV1Network_V2Network_Crosswalk")
#' gridcode  <- nhd_plus_load(1, "NHDPlusCatchment", "featuregridcode")
#' }
nhd_plus_load <- function(vpu, component = "NHDSnapshot", dsn,
                          file_ext = NA){

  if(!(file_ext %in% c(NA, "shp", "dbf"))){
    stop(paste0("file_ext must be set to either 'shp' or 'dbf'"))
  }

  nhd_plus_load_vpu <- function(vpu, component, dsn, ...){
    vpu_path <- file.path(nhd_path(), "NHDPlus",
                          basename(get_plus_remotepath(vpu, component)))

    if(any(!file.exists(vpu_path))){
      userconsents <- utils::menu(c("Yes", "No"),
        title = paste0(vpu, " vpu file not found. Download it?"))
      if(userconsents == 1){
        nhd_plus_get(vpu = vpu, component = component)
      }else{
        stop("No file. Cannot load.")
      }
    }

    candidate_files <- nhd_plus_list(vpu, component = component,
                                     full.names = TRUE, file_ext = file_ext)
    res <- candidate_files[grep(paste0(tolower(dsn), "\\."),
                                tolower(candidate_files))]
    if(length(res) == 0){
      stop(paste0("layer '", dsn, "' not found in component '",
                  component, "'"))
    }

    if(length(grep(paste0("shp", "$"), res)) > 0){
      res <- res[grep("shp$", res)]
      res <- sf::st_zm(sf::st_read(res, stringsAsFactors = FALSE, ...))
      is_spatial <- TRUE
      list(res = res, is_spatial = is_spatial)
    }else{
      res <- lapply(res, foreign::read.dbf, as.is = TRUE)
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
                      component = component, dsn = dsn, quiet = TRUE)$res))
    sf::st_crs(res) <- prj
  }

  res
}
