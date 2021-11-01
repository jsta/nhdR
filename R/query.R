#' Select NHDplus features via polygon or circular buffer of coordinate pair
#'
#' @export
#' @param lon numeric longitude. optional
#' @param lat numeric latitude. optional
#' @param poly sfc polygon. optional
#' @param dsn character data source
#' @param buffer_dist numeric buffer in units of coordinate degrees
#' @param approve_all_dl logical blanket approval to download all missing data. 
#'  Defaults to TRUE if session is non-interactive.
#' @param temporary logical set FALSE to save data to a persistent
#'  rappdirs location
#' @param ... parameters passed on to sf::st_read
#' @return A list of sf spatial objects
#'
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
#'   dsn = c("NHDWaterbody", "NHDFlowLine"),
#'   buffer_dist = units::as_units(5, "km"))
#'
#' qry <- nhd_plus_query(wk$Lon, wk$Lat,
#'   dsn = c("NHDWaterbody", "NHDFlowLine"), buffer_dist = 0.05)
#'
#' plot(qry$sp$NHDWaterbody$geometry, col = "blue")
#' plot(qry$sp$NHDFlowLine$geometry, col = "cyan", add = TRUE)
#' plot(qry$pnt, col = "red", pch = 19, add = TRUE)
#' axis(1)
#' axis(2)
#'
#' library(ggplot2)
#' ggplot(qry$sp$NHDWaterbody) + geom_sf()
#'
#' # query with a polygon
#' wbd <- qry$sp$NHDWaterbody[which.max(st_area(qry$sp$NHDWaterbody)), ]
#' qry_lines <- nhd_plus_query(poly = st_as_sfc(st_bbox(wbd)),
#'   dsn = "NHDFlowLine")
#' ggplot() +
#'   geom_sf(data = qry$sp$NHDWaterbody) +
#'   geom_sf(data = qry_lines$sp$NHDFlowLine, color = "red")
#' }
nhd_plus_query <- function(lon = NA, lat = NA, poly = NA,
                           dsn, buffer_dist = units::as_units(1, "km"),
                           approve_all_dl = FALSE, temporary = TRUE, ...) {

  if (!interactive()) {
    approve_all_dl <- TRUE
  }

  if (all(!is.na(c(lon, lat, poly))) | all(is.na(c(lon, lat, poly)))) {
    stop("Must specify either lon and lat or poly but not both.")
  }

  # ! in default buffer size for query or extract
  if (all(!is.na(poly)) & !(buffer_dist %in% c(0.01, 0.05))) {
    stop("Passing a polygon object returns only polygon-intersecting lines and disregards any buffer_dist setting.")
  }

  if (length(lon) > 1 | length(lat) > 1) {
    stop("nhd_plus_query only accepts a single lon-lat pair.")
  }

  crs_code <- 4326
  if (inherits(buffer_dist, "units")) {
    crs_code <- st_crs(albers_conic())
  }

  if (all(!is.na(c(lon, lat)))) {
    pnt         <- st_sfc(st_point(c(lon, lat)))
    st_crs(pnt) <- 4326
    vpu         <- find_vpu(
      st_transform(
        sf::st_buffer(
          st_transform(pnt, crs_code),
          buffer_dist),
        st_crs(nhdR::vpu_shp)))
    pnt <- st_transform(pnt, crs_code)

    pnt_buff  <- sf::st_sfc(sf::st_buffer(pnt, dist = buffer_dist))
    sf::st_crs(pnt_buff) <- sf::st_crs(pnt)
    utm_zone <- long2UTM(sf::st_coordinates(
      sf::st_transform(pnt, 4326))[1])
    crs <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84")
    pnt      <- sf::st_transform(pnt, crs = crs)
    pnt_buff <- sf::st_transform(pnt_buff, crs = crs)
    crs_dsn  <- sf::st_crs(
      nhd_plus_load_vpu(vpu[[1]], component = "NHDSnapshot",
        force_dl = FALSE,
        dsn = dsn, pretty = FALSE,
        quiet = TRUE, wkt_filter = NA,
        approve_all_dl = TRUE,
        temporary = temporary,
        query = paste0("SELECT * from ", dsn, " LIMIT 1"))$res
    )
    
    wkt_filter <- sf::st_as_text(
      sf::st_transform(sf::st_geometry(pnt_buff), crs_dsn))
  } else {
    poly <- st_transform(poly, st_crs(nhdR::vpu_shp))
    vpu  <- find_vpu(poly)

    utm_zone  <- long2UTM(
      sf::st_coordinates(
        st_transform(poly, 4326))[1])
    crs       <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84")
    poly      <- st_transform(poly, crs = crs)
    crs_dsn  <- sf::st_crs(
      nhd_plus_load_vpu(vpu[[1]], component = "NHDSnapshot",
        force_dl = FALSE,
        dsn = dsn, pretty = FALSE,
        quiet = TRUE, wkt_filter = NA,
        approve_all_dl = approve_all_dl,
        temporary = temporary,
        query = paste0("SELECT * from ", dsn, " LIMIT 1"))$res
    )
    wkt_filter <- sf::st_as_text(
      sf::st_transform(sf::st_geometry(poly), crs_dsn))
  }

  sp_sub <- lapply(dsn, function(x) nhd_plus_load(vpu = vpu, dsn = x,
    approve_all_dl = approve_all_dl, temporary = temporary,
    wkt_filter = wkt_filter,
    ...))
  names(sp_sub) <- dsn

  if (any(unlist(lapply(sp_sub, function(r) length(r$res))) > 0)) {
    if (all(class(sp_sub) == "list")) {
      sp_sub <- lapply(sp_sub, function(x) sf::st_transform(x, crs))
    } else {
      sp_sub <- sf::st_transform(sp_sub, crs)
    }
  }

  list(sp = sp_sub)
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
#' axis(1)
#' axis(2)
#' }
nhd_query <- function(lon, lat, dsn, buffer_dist = 0.05) {

  pnt         <- st_sfc(st_point(c(lon, lat)))
  st_crs(pnt) <- st_crs(nhdR::vpu_shp)

  state     <- find_state(pnt)
  state_abb <- datasets::state.abb[tolower(datasets::state.name) == state]

  sp        <- lapply(dsn, function(x) nhd_load(state = state_abb, dsn = x))
  names(sp) <- dsn

  sp_sub    <- select_point_overlay(
    pnt = pnt, sp = sp, buffer_dist = buffer_dist)

  pnt       <- st_transform(pnt, sf::st_crs(sp_sub[[1]]))

  list(pnt = pnt, sp = sp_sub)
}

#' Select features clipped by a point buffer around a point
#'
#' @param pnt geographic point of class sfc
#' @param sp list of sf data frames
#' @param buffer_dist numeric buffer in units of coordinate degrees
#' @return A list of sf spatial objects
#' @export
#' @examples \dontrun{
#' wk <- wikilake::lake_wiki("Gull Lake (Michigan)")
#' pnt <- sf::st_sfc(sf::st_point(c(wk$Lon, wk$Lat)))
#' sf::st_crs(pnt) <- 4326
#' sp <- lapply(c("NHDWaterbody", "NHDFlowLine"),
#'   function(x) nhd_plus_load(vpu = 4, dsn = x))
#' names(sp) <- c("NHDWaterbody", "NHDFlowLine")
#' qry <- select_point_overlay(pnt = pnt, sp = sp, buffer_dist = 0.05)
#' plot(qry$NHDWaterbody$geometry)
#' }
select_point_overlay <- function(pnt, sp, buffer_dist = 0.05) {

  pnt_buff  <- sf::st_sfc(sf::st_buffer(pnt, dist = buffer_dist))
  sf::st_crs(pnt_buff) <- sf::st_crs(pnt) # <- sf::st_crs(nhdR::vpu_shp)

  utm_zone <- long2UTM(sf::st_coordinates(
    sf::st_transform(pnt, 4326))[1])

  crs <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84")

  pnt      <- sf::st_transform(pnt, crs = crs)
  pnt_buff <- sf::st_transform(pnt_buff, crs = crs)

  if (all(class(sp) == "list")) {
    sp    <- lapply(sp, function(x) sf::st_transform(x, crs = crs))

    sp_intersecting <- lapply(sp,
      function(x) unlist(lapply(
        sf::st_intersects(x, pnt_buff), length)) > 0)

    sp_sub <- lapply(seq_len(length(sp_intersecting)),
      function(x) sp[[x]][sp_intersecting[[x]], ])
    names(sp_sub) <- names(sp)
  } else {
    sp <- sf::st_transform(sp, crs = crs)
    sp_intersecting <- unlist(lapply(
      sf::st_intersects(sp, pnt_buff), length)) > 0

    sp_sub <- sp[sp_intersecting, ]
  }

  sp_sub
}

#' Select features clipped by a polygon
#'
#' @param poly sf *polygon object
#' @param sp list of sf data frames
#' @return A list of sf spatial objects
#'
#' @importFrom sf st_crs st_coordinates st_transform st_intersects
#' @export
#'
select_poly_overlay <- function(poly, sp) {

  utm_zone  <- long2UTM(
    sf::st_coordinates(
      st_transform(poly, 4326))[1])
  crs       <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84")
  poly      <- st_transform(poly, crs = crs)

  if (all(class(sp) == "list")) {
    sp    <- lapply(sp, function(x) sf::st_transform(x, crs = crs))

    sp_intersecting <- lapply(sp,
      function(x) unlist(lapply(
        sf::st_intersects(x, poly), length)) > 0)

    sp_sub <- lapply(seq_len(length(sp_intersecting)),
      function(x) sp[[x]][sp_intersecting[[x]], ])
    names(sp_sub) <- names(sp)
  } else {
    sp <- sf::st_transform(sp, crs = crs)
    sp_intersecting <- unlist(lapply(
      sf::st_intersects(sp, poly), length)) > 0

    sp_sub <- sp[sp_intersecting, ]
  }

  sp_sub
}