#' Return terminal reaches from collection intersecting lake
#'
#' @param lon numeric decimal degree longitude
#' @param lat numeric decimal degree latitude
#' @param network sf lines collection
#' @param approve_all_dl logical blanket approval to download all missing data
#'
#' @export
#' @importFrom sf st_area st_centroid st_union st_crs st_sfc st_point st_crs<-
#' @importFrom rlang .data
#'
#' @examples \dontrun{
#' coords <- data.frame(lat = 20.79722, lon = -156.47833)
#' terminal_reaches(coords$lon, coords$lat)
#'
#' coords <- data.frame(lat = 41.42217, lon = -73.24189)
#' terminal_reaches(coords$lon, coords$lat)
#'
#' network <- nhd_plus_query(lon = coords$lon, lat = coords$lat,
#'                      dsn = "NHDFlowline", buffer_dist = 0.02)$sp$NHDFlowline
#' t_reach <- terminal_reaches(network = network)
#'
#' plot(network$geometry)
#' plot(t_reach$geometry, col = "red", add = TRUE)
#' }
terminal_reaches <- function(lon = NA, lat = NA, network = NA,
                             approve_all_dl = FALSE){

  if(all(is.na(network))){
    pnt         <- st_sfc(st_point(c(lon, lat)))
    st_crs(pnt) <- st_crs(nhdR::vpu_shp)
    vpu         <- find_vpu(pnt)

    poly <- nhd_plus_query(lon, lat, dsn = "NHDWaterbody",
                           buffer_dist = 0.01,
                           approve_all_dl = approve_all_dl)$sp$NHDWaterbody
    poly <- poly[which.max(st_area(poly)),] # find lake polygon
    network_lines <- nhd_plus_query(poly = poly,
                                    dsn = "NHDFlowline")$sp$NHDFlowline
  }else{
    network_lines <- network
    vpu <- find_vpu(st_centroid(st_union(network_lines)))
  }

  network_table <- nhd_plus_load(vpu = vpu, "NHDPlusAttributes",
                                 "PlusFlow", approve_all_dl = approve_all_dl)
  names(network_table) <- tolower(names(network_table))
  names(network_lines) <- tolower(names(network_lines))

  network_table <- dplyr::filter(network_table,
                                 .data$fromcomid %in% network_lines$comid |
                                   .data$tocomid %in% network_lines$comid)

  # find nodes with no downstream connections and at least one upstream conn.
  res <- dplyr::filter(network_table,
                       !(network_table$tocomid %in% network_table$fromcomid))
  up_one <- network_table[network_table$tocomid  %in% res$fromcomid,]
  res <- res[which(up_one$fromcomid != 0),]

  dplyr::filter(network_lines, .data$comid %in% res$fromcomid)
}

#' Return leaf reaches from a network or query intersecting lake
#'
#' @inheritParams terminal_reaches
#'
#' @export
#' @importFrom sf st_area st_centroid st_union
#' @importFrom rlang .data
#'
#' @examples \dontrun{
#' coords <- data.frame(lat = 20.79722, lon = -156.47833)
#' leaf_reaches(coords$lon, coords$lat)
#'
#' coords  <- data.frame(lat = 41.42217, lon = -73.24189)
#' l_reach <- leaf_reaches(coords$lon, coords$lat)
#'
#' network <- nhd_plus_query(lon = coords$lon, lat = coords$lat,
#'                           dsn = "NHDFlowline", buffer_dist = 0.02)$sp$NHDFlowline
#' l_reach <- leaf_reaches(network = network)
#'
#' plot(network$geometry)
#' plot(l_reach$geometry, col = "red", add = TRUE)
#' }
leaf_reaches <- function(lon = NA, lat = NA, network = NA,
                         approve_all_dl = FALSE){

  if(all(is.na(network))){
    pnt <- sf::st_sfc(sf::st_point(c(lon, lat)))
    sf::st_crs(pnt) <- sf::st_crs(nhdR::vpu_shp)
    vpu <- find_vpu(pnt)

    poly <- nhd_plus_query(lon, lat, dsn = "NHDWaterbody",
                           buffer_dist = 0.01, approve_all_dl = approve_all_dl)$sp$NHDWaterbody
    poly <- poly[which.max(st_area(poly)),] # find lake polygon
    network_lines <- nhd_plus_query(poly = poly,
                                    dsn = "NHDFlowline")$sp$NHDFlowline
  }else{
    network_lines <- network
    vpu <- find_vpu(st_centroid(st_union(network_lines)))
  }

  network_table <- nhd_plus_load(vpu = vpu, "NHDPlusAttributes",
                                 "PlusFlow", approve_all_dl = approve_all_dl)
  names(network_table) <- tolower(names(network_table))
  names(network_lines) <- tolower(names(network_lines))

  # trim to network lines
  network_table_focal <- dplyr::filter(network_table,
                                       .data$fromcomid %in% network_lines$comid |
                                         .data$tocomid %in% network_lines$comid)

  # find nodes with upstream connections but not in the focal set
  up_one <- network_table[network_table$tocomid  %in% network_table_focal$fromcomid,]
  res <- up_one[!(up_one$fromcomid %in% network_table_focal$tocomid),]

  res <- res[which(res$tocomid != 0 & res$fromcomid != 0),]

  dplyr::filter(network_lines, .data$comid %in% res$tocomid)

  # plot(network_lines$geometry)
  # plot(res$geometry, col = "red", add = TRUE)
}


#' Return stream network upstream of a waterbody
#'
#' @inheritParams terminal_reaches
#' @param maxsteps maximum number of stream climbing iterations
#'
#' @export
#'
#' @examples \dontrun{
#' library(mapview)
#' library(sf)
#'
#' coords <- data.frame(lat = 20.79722, lon = -156.47833)
#' res <- extract_network(coords$lon, coords$lat, maxsteps = 9)
#'
#' mapview(res)
#' }
extract_network <- function(lon = NA, lat = NA, maxsteps = 3,
                            approve_all_dl = FALSE){

  # retrieve network table
  pnt             <- st_sfc(st_point(c(lon, lat)))
  st_crs(pnt)     <- st_crs(nhdR::vpu_shp)
  vpu             <- find_vpu(pnt)
  network_table   <- nhd_plus_load(vpu = vpu, "NHDPlusAttributes",
                                 "PlusFlow", approve_all_dl = approve_all_dl)
  names(network_table) <- tolower(names(network_table))


  t_reaches     <- terminal_reaches(lon, lat)
  temp_reaches  <- neighbors(t_reaches$comid, network_table, direction = "up")
  res_reaches   <- temp_reaches

  all_terminal <- FALSE
  steps        <- 0
  while(!all_terminal){
    temp_reaches <- neighbors(temp_reaches$fromcomid,
                              network_table, direction = "up")
    if(nrow(temp_reaches) == 0 | steps == maxsteps){
      all_terminal <- TRUE
    }else{
     res_reaches <- rbind(res_reaches, temp_reaches)
     steps       <- steps + 1
    }
  }

  # pull geospatial lines
  lines_file <- nhd_plus_list(vpu, "NHDSnapshot", full.names = TRUE,
                              file_ext = "shp")
  lines_file <- lines_file[grep("NHDFlowline", lines_file)]

  # load full network for now evetually speed up with sql
  lines        <- nhd_plus_load(vpu, "NHDSnapshot", "NHDFlowline")
  names(lines) <- tolower(names(lines))

  dplyr::filter(lines, .data$comid %in% res_reaches$tocomid)
}

neighbors <- function(node, network_table, direction = c("up", "down")) {
  if(direction == "down"){
    stop("not implemented yet")
  }
  if (direction == "up"){
    res <- dplyr::filter(network_table, .data$tocomid %in% node)
    dplyr::filter(res, .data$fromcomid != 0)
  }
}
