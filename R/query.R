#' nhd_plus_query
#' @export
#' @param lon numeric longitude
#' @param lat numeric latitude
#' @param dsn character data source
#' @param buffer_dist numeric buffer in units of coordinate degrees
#' @examples \dontrun{
#'
#' wk <- wikilake::lake_wiki("Gull Lake (Michigan)")
#' qry <- nhd_plus_query(wk$Lon, wk$Lat,
#'          dsn = c("NHDWaterbody", "NHDFlowLine"), buffer_dist = 0.05)
#'
#' plot(qry$sp$NHDWaterbody$geometry, col = "blue")
#' plot(qry$sp$NHDFlowLine$geometry, col = "cyan", add = TRUE)
#' plot(qry$pnt, col = "red", pch = 19, add = TRUE)
#' axis(1); axis(2)
#'
#' library(ggplot2)
#' sf::st_crs(qry$sp$NHDWaterbody)
#' ggplot(qry$sp$NHDWaterbody) + geom_sf()
#' }

nhd_plus_query <- function(lon, lat, dsn, buffer_dist = 0.05){

  pnt <- sf::st_sfc(sf::st_point(c(lon, lat)))
  sf::st_crs(pnt) <- sf::st_crs(nhdR::vpu_shp)
  vpu <- find_vpu(pnt)

  sp <- lapply(dsn, function(x) nhd_plus_load(vpu = vpu, dsn = x))
  names(sp) <- dsn

  sp_sub <- select_point_overlay(pnt = pnt, sp = sp, buffer_dist = buffer_dist)

  pnt <- sf::st_transform(pnt, sf::st_crs(sp_sub[[1]]))
  list(pnt = pnt, sp = sp_sub)
}

#' nhd_query
#' @export
#' @import datasets
#' @param lon numeric longitude
#' @param lat numeric latitude
#' @param dsn character data source
#' @param buffer_dist numeric buffer in units of coordinate degrees
#' @examples \dontrun{
#' wk <- wikilake::lake_wiki("Worden Pond")
#' qry <- nhd_query(wk$Lon, wk$Lat, dsn = c("NHDWaterbody", "NHDFlowline"))
#'
#' plot(sf::st_geometry(qry$sp$NHDWaterbody), col = "blue")
#' plot(sf::st_geometry(qry$sp$NHDFlowline), col = "cyan", add = TRUE)
#' plot(qry$pnt, col = "red", pch = 19, add = TRUE)
#' axis(1); axis(2)
#' }

nhd_query <- function(lon, lat, dsn, buffer_dist = 0.05){

  pnt <- sf::st_sfc(sf::st_point(c(lon, lat)))
  sf::st_crs(pnt) <- sf::st_crs(nhdR::vpu_shp)

  state <- find_state(pnt)
  state_abb <- datasets::state.abb[tolower(datasets::state.name) == state]

  sp <- lapply(dsn, function(x) nhd_load(state = state_abb, layer_name = x))
  names(sp) <- dsn

  sp_sub <- select_point_overlay(pnt = pnt, sp = sp, buffer_dist = buffer_dist)

  pnt <- sf::st_transform(pnt, sf::st_crs(sp_sub[[1]]))

  list(pnt = pnt, sp = sp_sub)
}

#' select_point_overlay
#' @param pnt geographic point of class sfc
#' @param sp list of sf data frames
#' @param buffer_dist numeric buffer in units of coordinate degrees
#' @export
#' @examples \dontrun{
#' wk <- wikilake::lake_wiki("Gull Lake (Michigan)")
#' pnt <- sf::st_sfc(sf::st_point(c(wk$Lon, wk$Lat)))
#' sp <- lapply(c("NHDWaterbody", "NHDFlowLine"),
#'           function(x) nhd_plus_load(vpu = 4, dsn = x))
#' names(sp) <- c("NHDWaterbody", "NHDFlowLine")
#' qry <- select_point_overlay(pnt = pnt, sp = sp, buffer_dist = 0.05)
#'
#'}
select_point_overlay <- function(pnt, sp, buffer_dist = 0.05){

  pnt_buff  <- sf::st_sfc(sf::st_buffer(pnt, dist = buffer_dist))
  sf::st_crs(pnt_buff) <- sf::st_crs(pnt) <- sf::st_crs(nhdR::vpu_shp)

  utm_zone <- long2UTM(sf::st_coordinates(pnt)[1])
  crs <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84")

  pnt      <- sf::st_transform(pnt, crs = crs)
  pnt_buff <- sf::st_transform(pnt_buff, crs = crs)

  if(all(class(sp) == "list")){
    sp    <- lapply(sp, function(x) sf::st_transform(x, crs = crs))

    sp_intersecting <- lapply(sp,
                              function(x) unlist(lapply(
                                sf::st_intersects(x, pnt_buff), length)) > 0)

    sp_sub <- lapply(seq_len(length(sp_intersecting)),
                     function(x) sp[[x]][sp_intersecting[[x]],])
    names(sp_sub) <- names(sp)
  }else{
    sp <- sf::st_transform(sp, crs = crs)
    sp_intersecting <- unlist(lapply(
                          sf::st_intersects(sp, pnt_buff), length)) > 0

    sp_sub <- sp[sp_intersecting,]
  }

  sp_sub
}
