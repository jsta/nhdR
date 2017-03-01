#' nhd_plus_query
#' @export
#' @examples \dontrun{
#' # Lake Lashaway
#' wk <- wikilake::lake_wiki("Gull Lake (Michigan)")
#' qry <- nhd_plus_query(wk$Lon, wk$Lat, dsn = "NHDWaterbody")
#'
#' plot(qry$sp$geometry, col = "blue")
#' plot(qry$pnt, col = "red", pch = 19, add = TRUE)
#' axis(1); axis(2)
#' }

nhd_plus_query <- function(lon, lat, dsn){
  pnt <- sf::st_sfc(sf::st_point(c(lon, lat)))
  pnt_buff  <- sf::st_sfc(sf::st_buffer(pnt, dist = 0.05))
  sf::st_crs(pnt_buff) <- sf::st_crs(pnt) <- sf::st_crs(nhdR::vpu_shp)

  vpu <- sf::st_covers(nhdR::vpu_shp$geometry, pnt)
  vpu <- nhdR::vpu_shp[which(vpu == 1),]
  vpu <- suppressWarnings(as.numeric(as.character(vpu$UnitID)))
  vpu <- vpu[!is.na(vpu)]

  sp <- nhd_plus_load(vpu = vpu, dsn = dsn)
  sf::st_crs(sp) <- 4269

  sp       <- sf::st_transform(sp, crs = "+proj=utm +zone=10 +datum=WGS84")
  pnt      <- sf::st_transform(pnt, crs = "+proj=utm +zone=10 +datum=WGS84")
  pnt_buff <- sf::st_transform(pnt_buff,
                crs = "+proj=utm +zone=10 +datum=WGS84")


  sp_intersecting <- unlist(
                      lapply(sf::st_intersects(sp, pnt_buff), length)) > 0
  # check if any(sp_intersecting)
  sp_sub <- sp[sp_intersecting,]

  list(pnt = pnt, sp = sp_sub)
}
