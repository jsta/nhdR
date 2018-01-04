#' Load NHD layers into current session
#'
#' @param state character state abbreviation
#' @param dsn character name of a NHD layer
#' @param file_ext character choice of "shp" for spatial data and
#' "dbf" or "gpkg" for non-spatial. optional
#' @param approve_all_dl logical blanket approval to download all missing data
#' @param ... arguments passed to sf::st_read
#'
#' @return Spatial simple features object or data frame depending on the dsn
#' type and value passed to file_ext
#' @importFrom sf st_read
#' @importFrom gdalUtils ogr2ogr
#' @importFrom rlang quo
#' @importFrom dplyr tbl select src_sqlite
#' @export
#'
#' @details This function will ask the user to approve downloading missing data
#'  unless approve_all_dl is set to TRUE.
#'
#' @examples \dontrun{
#' dt <- nhd_load(c("RI"), c("NHDWaterbody"))
#' dt <- nhd_load(c("CT", "RI"), "NHDWaterbody")
#' dt <- nhd_load(c("CT", "RI"), "NHDWaterbody", quiet = TRUE)
#' dt <- nhd_load("MI", "NHDFlowline")
#' dt <- nhd_load("RI", "NHDReachCrossReference")
#' dt <- nhd_load("RI", "NHDWaterbody", file_ext = "dbf")
#' dt <- nhd_load(c("RI", "DC"), "NHDWaterbody", file_ext = "gpkg")
#' }
nhd_load <- function(state, dsn, file_ext = NA, approve_all_dl = FALSE, ...){

  if(!(file_ext %in% c(NA, "shp", "dbf", "gpkg"))){
    stop(paste0("file_ext must be set to either 'shp' or 'dbf'"))
  }

  nhd_state_exists <- function(state){
    if(any(!file.exists(gdb_path(state)))){
      state_exists <- 0
      if(!approve_all_dl){
        userconsents <- utils::menu(c("Yes", "No"),
                          title = paste0(state,
                          " state gdb file not found. Download it?"))
      }else{
        userconsents <- 1
      }
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
            sf::st_zm(sf::st_read(gdb_path(state), dsn,
              stringsAsFactors = FALSE, ...))},
          error = function(e) {
            temp_dir <- tempdir()
            gdalUtils::ogr2ogr(gdb_path(state), temp_dir, dsn)
            read.dbf(file.path(temp_dir, paste0(dsn, ".dbf")))
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
            res <- dplyr::tbl(dplyr::src_sqlite(gpkg_path), dsn)
            geom <- rlang::quo("geom")
            data.frame(dplyr::select(res, -geom))
          }else{
            temp_dir <- tempdir()
            gdalUtils::ogr2ogr(gdb_path(state), temp_dir, dsn)
            read.dbf(file.path(temp_dir, paste0(dsn, ".dbf")))
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

#' Load NHDplus layers into current session
#'
#' @param vpu numeric vector processing unit
#' @param component character component name
#' @param dsn data source name
#' @param file_ext character choice of "shp" for spatial data and
#' "dbf" for non-spatial. optional
#' @param approve_all_dl logical blanket approval to download all missing data
#' @return spatial object
#' @importFrom sf st_read st_zm
#' @importFrom foreign read.dbf
#' @importFrom curl has_internet
#' @export
#'
#' @details This function will ask the user to approve downloading missing data
#' unless approve_all_dl is set to TRUE.
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
#' flowline_vaa  <- nhd_plus_load(1, "NHDPlusAttributes", "PlusFlowlineVAA")
#'
#' # Character VPU
#' plusflow <- nhd_plus_load(vpu = "10L", "NHDPlusAttributes", "PlusFlow")
#' }
nhd_plus_load <- function(vpu, component = "NHDSnapshot", dsn,
                          file_ext = NA, approve_all_dl = FALSE){

  if(!(file_ext %in% c(NA, "shp", "dbf"))){
    stop(paste0("file_ext must be set to either 'shp' or 'dbf'"))
  }

  nhd_plus_load_vpu <- function(vpu, component, dsn, ...){
      vpu_path <- list.files(file.path(nhd_path(), "NHDPlus"),
                             include.dirs = TRUE, full.names = TRUE)
      vpu_path <- vpu_path[grep(vpu, vpu_path)]
      vpu_path <- vpu_path[
        seq_len(length(vpu_path)) %in% grep("7z", vpu_path)]
      vpu_path <- vpu_path[grep(component, vpu_path)]

    if(any(!file.exists(vpu_path)) | length(vpu_path) == 0){

      if(!approve_all_dl){
        userconsents <- utils::menu(c("Yes", "No"),
                  title = paste0(vpu, " vpu file not found. Download it?"))
      }else{
        userconsents <- 1
      }

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
