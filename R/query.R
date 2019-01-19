#' Select NHDplus features via polygon or circular buffer of coordinate pair
#'
#' @export
#' @param lon numeric longitude. optional
#' @param lat numeric latitude. optional
#' @param poly sfc polygon. optional
#' @param dsn character data source
#' @param buffer_dist numeric buffer in units of coordinate degrees
#' @param approve_all_dl logical blanket approval to download all missing data
#' @param ... parameters passed on to sf::st_read
#' @examples \dontrun{
#' library(sf)
#' wk <- wikilake::lake_wiki("Gull Lake (Michigan)")
#'
#' pnt <- st_as_sf(wk, coords = c("Lon", "Lat"), crs = 4326)
#' pnt <- st_transform(pnt, st_crs(vpu_shp))
#' # nhd_plus_list(nhdR::find_vpu(pnt))
#'
#' # set a non-geographic (projected) buffer size
#' qry <- nhd_plus_query(wk$Lon, wk$Lat,
#'         dsn = c("NHDWaterbody", "NHDFlowLine"),
#'         buffer_dist = units::as_units(5, "km"))
#'
#' qry <- nhd_plus_query(wk$Lon, wk$Lat,
#'          dsn = c("NHDWaterbody", "NHDFlowLine"), buffer_dist = 0.05)
#'
#' plot(qry$sp$NHDWaterbody$geometry, col = "blue")
#' plot(qry$sp$NHDFlowLine$geometry, col = "cyan", add = TRUE)
#' plot(qry$pnt, col = "red", pch = 19, add = TRUE)
#' axis(1); axis(2)
#'
#' library(ggplot2)
#' ggplot(qry$sp$NHDWaterbody) + geom_sf()
#'
#' # query with a polygon
#' wbd <- qry$sp$NHDWaterbody[which.max(st_area(qry$sp$NHDWaterbody)),]
#' qry_lines <- nhd_plus_query(poly = st_as_sfc(st_bbox(wbd)),
#'                             dsn = "NHDFlowLine")
#' ggplot() +
#'   geom_sf(data = qry$sp$NHDWaterbody) +
#'   geom_sf(data = qry_lines$sp$NHDFlowLine, color = "red")
#' }

nhd_plus_query <- function(lon = NA, lat = NA, poly = NA,
                           dsn, buffer_dist = 0.05, approve_all_dl = FALSE, ...){

  if(all(!is.na(c(lon, lat, poly)))){
    stop("Must specify either lon and lat or poly but not both.")
  }

  if(!is.na(poly) & buffer_dist != 0.05){
    stop("Passing a polygon object returns only polygon-intersecting lines and disregards any buffer_dist setting.")
  }

  crs_code <- 4326
  if(inherits(buffer_dist, "units")){
    crs_code <- st_crs(albers_conic())
  }

  if(all(!is.na(c(lon, lat)))){
    pnt         <- st_sfc(st_point(c(lon, lat)))
    st_crs(pnt) <- 4326
    vpu         <- find_vpu(
      st_transform(
        sf::st_buffer(
          st_transform(pnt, crs_code),
          buffer_dist),
        st_crs(nhdR::vpu_shp)))
    pnt <- st_transform(pnt, crs_code)

    sp <- lapply(dsn, function(x) nhd_plus_load(vpu = vpu, dsn = x,
                                          approve_all_dl = approve_all_dl, ...))
    names(sp) <- dsn

    sp_sub <- select_point_overlay(pnt = pnt, sp = sp,
                                   buffer_dist = buffer_dist)

    pnt <- st_transform(pnt, sf::st_crs(sp_sub[[1]]))
    list(pnt = pnt, sp = sp_sub)
  }else{

    poly <- st_transform(poly, st_crs(nhdR::vpu_shp))
    vpu  <- find_vpu(poly)

    sp <- lapply(dsn, function(x) nhd_plus_load(vpu = vpu, dsn = x,
                                          approve_all_dl = approve_all_dl, ...))
    names(sp) <- dsn

    sp_sub <- select_poly_overlay(poly = poly, sp = sp)

    list(sp = sp_sub)
  }
}

#' Select NHD features clipped by a circular buffer a coordinate pair
#'
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

  pnt         <- st_sfc(st_point(c(lon, lat)))
  st_crs(pnt) <- st_crs(nhdR::vpu_shp)

  state     <- find_state(pnt)
  state_abb <- datasets::state.abb[tolower(datasets::state.name) == state]

  sp        <- lapply(dsn, function(x) nhd_load(state = state_abb, dsn = x))
  names(sp) <- dsn

  sp_sub    <- select_point_overlay(pnt = pnt, sp = sp, buffer_dist = buffer_dist)

  pnt       <- st_transform(pnt, sf::st_crs(sp_sub[[1]]))

  list(pnt = pnt, sp = sp_sub)
}

#' Select features clipped by a point buffer around a point
#'
#' @param pnt geographic point of class sfc
#' @param sp list of sf data frames
#' @param buffer_dist numeric buffer in units of coordinate degrees
#' @export
#' @examples \dontrun{
#' wk <- wikilake::lake_wiki("Gull Lake (Michigan)")
#' pnt <- sf::st_sfc(sf::st_point(c(wk$Lon, wk$Lat)))
#' sf::st_crs(pnt) <- 4326
#' sp <- lapply(c("NHDWaterbody", "NHDFlowLine"),
#'           function(x) nhd_plus_load(vpu = 4, dsn = x))
#' names(sp) <- c("NHDWaterbody", "NHDFlowLine")
#' qry <- select_point_overlay(pnt = pnt, sp = sp, buffer_dist = 0.05)
#' plot(qry$NHDWaterbody$geometry)
#'
#'}
select_point_overlay <- function(pnt, sp, buffer_dist = 0.05){

  pnt_buff  <- sf::st_sfc(sf::st_buffer(pnt, dist = buffer_dist))
  sf::st_crs(pnt_buff) <- sf::st_crs(pnt) # <- sf::st_crs(nhdR::vpu_shp)

  utm_zone <- long2UTM(sf::st_coordinates(
    sf::st_transform(pnt, 4326))[1])

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

#' Select features clipped by a polygon
#'
#' @param poly sf *polygon object
#' @param sp list of sf data frames
#'
#' @importFrom sf st_crs st_coordinates st_transform st_intersects
#' @export
#'
select_poly_overlay <- function(poly, sp){

  utm_zone  <- long2UTM(
    sf::st_coordinates(
      st_transform(poly, 4326))[1])
  crs       <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84")
  poly      <- st_transform(poly, crs = crs)

  if(all(class(sp) == "list")){
    sp    <- lapply(sp, function(x) sf::st_transform(x, crs = crs))

    sp_intersecting <- lapply(sp,
                              function(x) unlist(lapply(
                                sf::st_intersects(x, poly), length)) > 0)

    sp_sub <- lapply(seq_len(length(sp_intersecting)),
                     function(x) sp[[x]][sp_intersecting[[x]],])
    names(sp_sub) <- names(sp)
  }else{
    sp <- sf::st_transform(sp, crs = crs)
    sp_intersecting <- unlist(lapply(
      sf::st_intersects(sp, poly), length)) > 0

    sp_sub <- sp[sp_intersecting,]
  }

  sp_sub
}
