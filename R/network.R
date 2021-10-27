#' Return terminal reaches from collection intersecting lake
#'
#' In the case of a network query, a terminal reach is a stream flowline that
#' has no downstream reaches in-network. In the case of a point query, a
#' terminal reach is a flowline that exits the intersecting surface waterbody.
#'
#' @details
#' There are multiple ways to execute \code{\link{terminal_reaches}}:
#' \itemize{
#'  \item Only providing lon + lat arguments - this will query the
#'  corresponding lake polygon layer and find the terminal reach of the lake
#'  intersecting a buffer around the specified point.
#'  \item Only providing a lake polygon - this is essentially the same as
#'  above except there is no preliminary lake polygon query.
#'  \item Only providing a network of stream lines - this provides the most
#'  downstream reach irrespective of lakes.
#' }
#'
#' @param lon numeric decimal degree longitude. optional. See Details section.
#' @param lat numeric decimal degree latitude. optional. See Details section.
#' @param network sf lines collection. optional. See Details section.
#' @param lakepoly sf polygon.  optional. See Details section.
#' @param buffer_dist numeric buffer around lat-lon point in dec. deg.
#' @param lakewise logical. If TRUE, return the terminal reaches of all lakes
#' in the stream network rather than a single terminal reach of the focal lake.
#' @param lakesize_threshold numeric above which to count as a lake (ha).
#' @param approve_all_dl logical blanket approval to download all missing data.
#' Defaults to TRUE if session is non-interactive.
#' @param temporary logical set FALSE to save data to a persistent
#'  rappdirs location
#' @param ... parameters passed on to sf::st_read
#' @return An sf data frame with LINESTRING geometries
#'
#' @export
#' @importFrom sf st_area st_centroid st_union st_crs st_sfc st_point st_crs<- st_cast st_convex_hull
#' @importFrom rlang .data
#' @importFrom units as_units
#'
#' @examples \dontrun{
#' library(sf)
#' library(mapview)
#'
#' coords <- data.frame(lat = 46.32711, lon = -89.58893)
#' t_reach <- terminal_reaches(coords$lon, coords$lat)
#'
#' coords <- data.frame(lat = 20.79722, lon = -156.47833)
#' # use a non-geographic (projected) buffer size
#' t_reach <- terminal_reaches(coords$lon, coords$lat,
#'   buffer_dist = units::as_units(5, "km"))
#'
#' coords  <- data.frame(lat = 42.96628, lon = -89.25264)
#' t_reach <- terminal_reaches(coords$lon, coords$lat)
#'
#' coords  <- data.frame(lat = 41.42217, lon = -73.24189)
#' t_reach <- terminal_reaches(coords$lon, coords$lat)
#'
#' mapview(st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)) +
#'   mapview(t_reach$geometry, color = "red")
#'
#' coords <- data.frame(lat = 41.859080, lon = -71.575422)
#' network <- nhd_plus_query(lon = coords$lon, lat = coords$lat,
#'   dsn = "NHDFlowline", buffer_dist = 0.05)$sp$NHDFlowline
#' t_reach      <- terminal_reaches(network = network)
#' t_reach_lake <- terminal_reaches(network = network, lakewise = TRUE,
#'   lakesize_threshold = 1)
#'
#' mapview(network) + mapview(t_reach_lake, color = "green") +
#'   mapview(t_reach, color = "red")
#' }
terminal_reaches <- function(lon = NA, lat = NA, buffer_dist = 0.01,
                             network = NA, lakepoly = NA, lakewise = FALSE,
                             lakesize_threshold = 4, approve_all_dl = FALSE,
                             temporary = TRUE,
                             ...) {

  if (!interactive()) {
    approve_all_dl <- TRUE
  }

  if (all(is.na(network))) {
    pnt         <- st_sfc(st_point(c(lon, lat)))
    st_crs(pnt) <- st_crs(nhdR::vpu_shp)
    vpu         <- find_vpu(pnt)

    poly <- nhd_plus_query(lon, lat, dsn = "NHDWaterbody",
      buffer_dist = buffer_dist,
      approve_all_dl = approve_all_dl, 
      temporary = temporary, ...)$sp$NHDWaterbody

    if (nrow(poly) == 0) {
      stop("No lake polygon found at query point")
    }

    # exclude great lakes --
    # is GNIS_NAME sometimes mixed case caps? Yes :(
    names(poly)[grep("GNIS_N", names(poly))] <- toupper(
      names(poly)[grep("GNIS_N", names(poly))])

    if (all(poly$GNIS_NAME %in% great_lakes()$GNIS_NAME) &
      nrow(poly) > 0) {
      stop(paste0("This point intersects one of the Great Lakes. ",
        "NHD doesn't support finding their terminal reach."))
    }

    poly <- poly[poly$FTYPE != "SwampMarsh", ]
    poly <- poly[!(poly$GNIS_NAME %in% great_lakes()$GNIS_NAME), ]
    poly <- make_valid_geom_s2(poly)
    poly <- poly[which.max(st_area(poly)), ] # find lake polygon

    if (nrow(poly) == 0) {
      stop("No lake polygon found at query point")
    }

    network_lines <- nhd_plus_query(poly = poly,
      dsn = "NHDFlowline",
      ...)$sp$NHDFlowline

    if (nrow(network_lines) == 0 | length(network_lines) == 0) {
      stop("No streams intersect this lake polygon")
    }
  } else {
    network_lines <- network
    vpu <- suppressWarnings(find_vpu(st_centroid(st_union(network_lines))))
  }

  # network_lines <- dplyr::filter(network_lines,
  #                                rlang::.data$FTYPE != "Coastline")

  network_table <- nhd_plus_load(vpu = vpu, "NHDPlusAttributes",
    "PlusFlow", approve_all_dl = approve_all_dl, temporary = temporary, ...)
  names(network_table) <- tolower(names(network_table))
  names(network_lines) <- tolower(names(network_lines))

  network_table <- dplyr::filter(network_table,
    .data$fromcomid %in% network_lines$comid |
      .data$tocomid %in% network_lines$comid)

  if (lakewise) {
    # pull lines that intersect a lake
    network_lines <- dplyr::filter(network_lines, .data$wbareacomi != 0)

    # filter lines to those that intersect a lake larger than size threshold
    poly <- suppressMessages(
      nhd_plus_query(poly = st_convex_hull(
        st_union(st_cast(network_lines, "MULTILINESTRING"))),
      dsn = "NHDWaterbody",
      buffer_dist = 0.01,
      approve_all_dl = approve_all_dl, 
      temporary = temporary,
      ...)$sp$NHDWaterbody)

    poly <- poly[st_area(poly) >
      units::as_units(lakesize_threshold, "ha"), ]
    poly <- st_transform(poly, st_crs(network_lines))

    intersecting_reaches <- network_lines[unlist(lapply(
      suppressMessages(st_intersects(network_lines, poly)),
      function(x) length(x) > 0)), ]

    network_table <- dplyr::filter(network_table,
      .data$fromcomid %in% intersecting_reaches$comid)

  }

  # find nodes with no downstream connections.
  res    <- dplyr::filter(network_table,
    !(network_table$tocomid %in% network_table$fromcomid))

  if (!lakewise) {
    # find nodes with at least one upstream conn.
    up_one <- network_table[network_table$tocomid  %in% res$fromcomid, ]
    res    <- res[which(up_one$fromcomid != 0), ]
  }

  # find nodes with an upstream reach with ftype == ArtificialPath?

  # find nodes with no downstream and at least one upstream conn.
  dplyr::filter(network_lines, .data$comid %in% res$fromcomid)
}

#' Return leaf reaches from a network or query intersecting lake
#'
#' A leaf reach is a stream flowline that has upstream connections but is
#' not in the focal set.
#'
#' @inheritParams terminal_reaches
#' @return An sf data frame with LINESTRING geometries
#' @export
#' @importFrom sf st_area st_centroid st_union
#' @importFrom rlang .data
#'
#' @examples \dontrun{
#' coords <- data.frame(lat = 20.79722, lon = -156.47833)
#' # nhd_plus_get(
#' #  nhdR::find_vpu(
#' #    sf::st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)),
#' # temporary = FALSE)
#' leaf_reaches(coords$lon, coords$lat)
#'
#' coords  <- data.frame(lat = 41.42217, lon = -73.24189)
#' l_reach <- leaf_reaches(coords$lon, coords$lat)
#'
#' network <- nhd_plus_query(lon = coords$lon, lat = coords$lat,
#'   dsn = "NHDFlowline", buffer_dist = 0.02)$sp$NHDFlowline
#' l_reach <- leaf_reaches(network = network)
#'
#' plot(network$geometry)
#' plot(l_reach$geometry, col = "red", add = TRUE)
#' }
leaf_reaches <- function(lon = NA, lat = NA, network = NA,
                         approve_all_dl = FALSE, temporary = TRUE, ...) {

  if (!interactive()) {
    approve_all_dl <- TRUE
  }

  if (all(is.na(network))) {
    pnt <- sf::st_sfc(sf::st_point(c(lon, lat)))
    sf::st_crs(pnt) <- sf::st_crs(nhdR::vpu_shp)
    vpu <- find_vpu(pnt)

    poly <- nhd_plus_query(lon, lat, dsn = "NHDWaterbody",
      buffer_dist = units::as_units(0.5, "km"),
      approve_all_dl = approve_all_dl,
      temporary = temporary, ...)$sp$NHDWaterbody
    poly          <- poly[which.max(st_area(poly)), ] # find lake polygon
    network_lines <- nhd_plus_query(poly = poly,
      dsn = "NHDFlowline", ...)$sp$NHDFlowline
  } else {
    network_lines <- network
    vpu <- suppressWarnings(
      find_vpu(
        st_centroid(st_union(network_lines)))
    )
  }

  network_table <- nhd_plus_load(vpu = vpu, "NHDPlusAttributes",
    "PlusFlow", approve_all_dl = approve_all_dl, temporary = temporary)
  names(network_table) <- tolower(names(network_table))
  names(network_lines) <- tolower(names(network_lines))

  # trim to network lines
  network_table_focal <- dplyr::filter(network_table,
    .data$fromcomid %in% network_lines$comid |
      .data$tocomid %in% network_lines$comid)

  # find nodes with upstream connections but not in the focal set
  up_one <- network_table[
    network_table$tocomid  %in% network_table_focal$fromcomid, ]
  res <- up_one[!(up_one$fromcomid %in% network_table_focal$tocomid), ]

  res <- res[which(res$tocomid != 0 & res$fromcomid != 0), ]

  dplyr::filter(network_lines, .data$comid %in% res$tocomid)

  # plot(network_lines$geometry)
  # plot(res$geometry, col = "red", add = TRUE)
}


#' Return nhd plus stream network upstream of a waterbody
#'
#' @details The lon and lat arguments are used for querying the corresponding
#' lake polygon layer which is then used to climb its intersecting stream
#' network.
#'
#' @param lon numeric decimal degree longitude
#' @param lat numeric decimal degree latitude
#' @inheritParams terminal_reaches
#' @param maxsteps maximum number of stream climbing iterations
#' @param lines sf spatial lines object to limit extent of the network search
#' @param ... parameters passed on to sf::st_read
#'
#' @return An sf data frame with LINESTRING geometries
#' @export
#'
#' @examples \dontrun{
#' library(mapview)
#' library(sf)
#'
#' # headwater lakes have no upstream network
#' coords <- data.frame(lat = 46.32711, lon = -89.58893)
#' res <- extract_network(coords$lon, coords$lat, maxsteps = 9)
#'
#' # fails if no lake nhdp lake found within the buffer at the query point
#' coords <- data.frame(lat = 43.62453, lon = -85.47164)
#' res <- extract_network(coords$lon, coords$lat, maxsteps = 9)
#'
#' coords <- data.frame(lat = 20.79722, lon = -156.47833)
#' # use a non-geographic (projected) buffer size
#' res <- extract_network(coords$lon, coords$lat, maxsteps = 9,
#'   buffer_dist = units::as_units(5, "km"))
#'
#' # use a projected buffer size
#' res <- extract_network(coords$lon, coords$lat, maxsteps = 9)
#'
#' # no upstream network for lakes intersecting the Great Lakes
#' coords <- data.frame(lat = 44.6265, lon = -86.23121)
#' res <- extract_network(coords$lon, coords$lat, maxsteps = 3)
#'
#' coords <- data.frame(lat = 42.96523, lon = -89.2527)
#' res <- extract_network(coords$lon, coords$lat, maxsteps = 9)
#'
#' mapview(res)
#' }
extract_network <- function(lon = NA, lat = NA, lines = NA,
                            buffer_dist = 0.01, maxsteps = 3,
                            approve_all_dl = FALSE, temporary = TRUE, ...) {

  if (!interactive()) {
    approve_all_dl <- TRUE
  }

  if (length(lon) > 1 | length(lat) > 1) {
    stop("extract_network only accepts a single lon-lat pair.")
  }

  # retrieve network table
  pnt             <- st_sfc(st_point(c(lon, lat)))
  st_crs(pnt)     <- st_crs(nhdR::vpu_shp)
  vpu             <- find_vpu(pnt)
  network_table   <- nhd_plus_load(vpu = vpu, "NHDPlusAttributes",
    "PlusFlow", approve_all_dl = approve_all_dl, temporary = temporary)
  names(network_table) <- tolower(names(network_table))

  if (all(!is.na(lines))) {
    names(lines) <- tolower(names(lines))
    # filter network table by line comids
    network_table <- dplyr::filter(network_table,
      .data$tocomid %in% lines$comid | .data$fromcomid %in% lines$comid)
  }

  t_reaches     <- terminal_reaches(lon, lat, buffer_dist = buffer_dist,
    lakewise = TRUE, pretty = TRUE,
    approve_all_dl = approve_all_dl, 
    temporary = temporary, ...)
  temp_reaches  <- neighbors(t_reaches$comid, network_table, direction = "up")
  res_reaches   <- temp_reaches

  if (nrow(t_reaches) == 0 | nrow(res_reaches) == 0) {
    message("lake is not connected to stream network or any upnetwork streams")
    return(NA)
  } else {
    all_terminal <- FALSE
    steps        <- 0
    while (!all_terminal) {
      temp_reaches <- neighbors(temp_reaches$fromcomid,
        network_table, direction = "up")
      if (nrow(temp_reaches) == 0 | steps == maxsteps) {
        all_terminal <- TRUE
      } else {
        res_reaches <- rbind(res_reaches, temp_reaches)
        steps       <- steps + 1
      }
    }

    # pull geospatial lines
    # load full network for now eventually speed up with sql
    # lines_file <- nhd_plus_list(vpu, "NHDSnapshot", full.names = TRUE,
    #                             file_ext = "shp")
    # lines_file <- lines_file[grep("NHDFlowline", lines_file)]
    if (all(is.na(lines))) {
      lines        <- nhd_plus_load(vpu, "NHDSnapshot", "NHDFlowline",
        pretty = TRUE, approve_all_dl = approve_all_dl, temporary = temporary, ...)
      names(lines) <- tolower(names(lines))
    }

    utm_zone <- long2UTM(sf::st_coordinates(pnt)[1])
    crs      <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84")

    res <- dplyr::filter(lines, .data$comid %in% res_reaches$tocomid)

    if (nrow(res) > 0) {
      res <- st_transform(res, crs = crs)
    } else { # the 'network' is a single reach
      res <- t_reaches
    }

    # pull first order streams
    l_reach <- leaf_reaches(
      network = res, pretty = TRUE, approve_all_dl = approve_all_dl, temporary = temporary)
    if (nrow(l_reach) > 0) {
      first_order_reaches <- neighbors(l_reach$comid, network_table,
        direction = "up")

      rbind(res, st_transform(dplyr::filter(lines,
        .data$comid %in% first_order_reaches$fromcomid), crs = crs))
    } else { # the 'network' is a single reach
      res
    }
  }
}

neighbors <- function(node, network_table, direction = c("up", "down")) {
  if (direction == "down") {
    stop("Traversing down the network is not implemented yet.")
  }
  if (direction == "up") {
    res <- dplyr::filter(network_table, .data$tocomid %in% node)
    dplyr::filter(res, .data$fromcomid != 0)
  }
}