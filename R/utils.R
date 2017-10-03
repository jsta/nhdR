nhd_path <- function(){
  path <- file.path(rappdirs::user_data_dir(appname = "nhdR",
                                      appauthor = "nhdR"))
  dir.create(path, showWarnings = FALSE)
  path
}

gdb_path <- function(state){
  paste0(nhd_path(), .Platform$file.sep, "NHDH_", state, ".gdb")
}

gdb_plus_path <- function(){
  file.path(nhd_path(),
            "NHDPlusV21_NationalData_National_Seamless_Geodatabase_02",
            "NHDPlusNationalData",
            "NHDPlusV21_National_Seamless.gdb")
}

# zip_plus_path <- function(){
#   file.path
#
# }

get_if_not_exists <- function(url, destfile){
  if(!file.exists(destfile)){
    message(paste0("Downloading ", url))
    suppressWarnings(
      httr::GET(url, httr::write_disk(destfile), httr::progress()))
    TRUE
  }else{
    message(paste0("A local copy of ", url, " already exists on disk"))
    FALSE
  }
}

zero_pad <- function(x, digits){
  if(nchar(x) < 2){
    paste0(paste0(rep(0, digits), collapse = ""), x, collapse = "")
  }else{
    x
  }
}

get_plus_remotepath <- function(vpu, component = "NHDSnapshot"){
  if(vpu == "National"){
    baseurl <- "http://www.horizon-systems.com/NHDPlus/V2NationalData.php"
  }else{
    baseurl <- paste0("http://www.horizon-systems.com/nhdplus/NHDPlusV2_",
                    zero_pad(vpu, 1), ".php")
  }

  res <- rvest::html_attrs(rvest::html_nodes(xml2::read_html(baseurl), "a"))
  res <- unlist(res[grep(component, res)])
  res <- res[!(seq_len(length(res)) %in% c(grep("FGDB", res), grep(".pdf", res)))][1]
  res
}

is_spatial <- function(filename){
  length(grep("shp$", filename)) > 0
}

#' Find VPU
#'
#' Find Vector Processing Unit from sf object
#'
#' @param pnt sf object
#'
#' @importFrom sf st_transform st_intersects
#' @export
#'
find_vpu <- function(pnt){
  pnt <- sf::st_transform(pnt, sf::st_crs(nhdR::vpu_shp))
  vpu <- nhdR::vpu_shp[nhdR::vpu_shp$UnitType == "VPU",]
  vpu_intersects <- sf::st_intersects(vpu, pnt)
  vpu <- vpu[which(sapply(vpu_intersects,
                          function(x) length(x > 0)) == 1),]
  vpu <- as.character(vpu$UnitID)
  vpu[!is.na(vpu)]
}

find_state <- function(pnt){
  state_data_sf <- sf::st_as_sf(map("state", plot = FALSE, fill = TRUE))
  res <- sf::st_transform(state_data_sf, sf::st_crs(pnt))

  res_intersects <- sf::st_intersects(res, pnt)

  state <- res$ID[
            which(unlist(lapply(res_intersects, length)) > 0)]
  state
}

handle_dbf <- function(state, layer_name){
  temp_dir <- tempdir()
  gdalUtils::ogr2ogr(gdb_path(state), temp_dir, layer_name)
  read.dbf(file.path(temp_dir, paste0(layer_name, ".dbf")))
}

# https://stackoverflow.com/a/9188972/3362993
long2UTM <- function(long) {
  (floor((long + 180)/6) %% 60) + 1
}

compile_gpkg <- function(state){
  gdalUtils::ogr2ogr(
    src_datasource_name = gdb_path(state),
    dst_datasource_name = gsub(".gdb", ".gpkg", gdb_path(state)),
    f = "GPKG")
}

is_gpkg_installed <- function(){
  name <- rlang::quo("name")
  all(as.logical(dplyr::filter(sf::st_drivers(), name == "GPKG")[3:6]))
}
