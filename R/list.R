#' nhd_list
#'
#' @export
#' @param state character state abbreviation
#' @importFrom rgdal ogrListLayers
#'
#' @examples \dontrun{
#' nhd_list(state = "DC")
#' }
nhd_list <- function(state){
  rgdal::ogrListLayers(gdb_path(state))
}

#' nhd_plus_list
#'
#' @export
#' @importFrom rgdal ogrListLayers
#'
#' @examples \dontrun{
#' nhd_list()
#' }
nhd_plus_list <- function(){
  rgdal::ogrListLayers(gdb_plus_path())
}

