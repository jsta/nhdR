#' nhd_plus_query
#' @export
#' @param lon numeric longitude
#' @param lat numeric latitude
#' @param dsn character data source
#' @param buffer_dist numeric buffer in units of coordinate degrees
#' @examples \dontrun{
#' # Lake Lashaway
#' wk <- wikilake::lake_wiki("Gull Lake (Michigan)")
#' qry <- nhd_plus_query(wk$Lon, wk$Lat, dsn = c("NHDWaterbody", "NHDFlowLine"))
#'
#' plot(qry$sp$NHDWaterbody$geometry, col = "blue")
#' plot(qry$sp$NHDFlowLine$geometry, col = "cyan", add = TRUE)
#' plot(qry$pnt, col = "red", pch = 19, add = TRUE)
#' axis(1); axis(2)
#' }

nhd_plus_query <- function(lon, lat, dsn, buffer_dist = 0.05){
  pnt <- sf::st_sfc(sf::st_point(c(lon, lat)))
  pnt_buff  <- sf::st_sfc(sf::st_buffer(pnt, dist = buffer_dist))
  sf::st_crs(pnt_buff) <- sf::st_crs(pnt) <- sf::st_crs(nhdR::vpu_shp)

  vpu <- sf::st_covers(nhdR::vpu_shp$geometry, pnt)
  vpu <- nhdR::vpu_shp[which(vpu == 1),]
  vpu <- suppressWarnings(as.numeric(as.character(vpu$UnitID)))
  vpu <- vpu[!is.na(vpu)]

  sp <- lapply(dsn, function(x) nhd_plus_load(vpu = vpu, dsn = x))
  for(i in 1:length(sp)){
    sf::st_crs(sp[[i]]) <- 4269
  }

  sp       <- lapply(sp, function(x) sf::st_transform(x,
                crs = "+proj=utm +zone=10 +datum=WGS84"))
  pnt      <- sf::st_transform(pnt, crs = "+proj=utm +zone=10 +datum=WGS84")
  pnt_buff <- sf::st_transform(pnt_buff,
                crs = "+proj=utm +zone=10 +datum=WGS84")

  sp_intersecting <- lapply(sp, function(x) unlist(lapply(sf::st_intersects(x, pnt_buff), length)) > 0)

  # check if any(sp_intersecting)
  sp_sub <- lapply(1:length(sp_intersecting),
                   function(x) sp[[x]][sp_intersecting[[x]],])
  names(sp_sub) <- dsn

  list(pnt = pnt, sp = sp_sub)
}
