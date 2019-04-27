
library(LAGOSNEgis)
library(fontr)
library(sf)
library(mapview)
library(nhdR)
library(ggplot2)
library(cowplot)
library(hexSticker)
library(sp)

# ---- function to intersect text glyph with watershed ----
# https://djnavarro.net/post/in-between.html

glyph_sf <- function(chars, bbox, scale_factor = 3700, offset_y = 0){
  # chars <- "nhdR"
  # bbox <- hu12
  n_raw_mat <- lapply(strsplit(chars, "")[[1]], function(x){
    n_raw       <- fontr::glyph_polygon(x, face = "bold")
    centroid    <- st_centroid(st_as_sfc(st_bbox(bbox)))
    centroid    <- as.data.frame(st_coordinates(centroid))
    # multiplication avoids rounding errors
    n_raw$x     <- (n_raw$x * 10) + centroid$X
    n_raw$y     <- (n_raw$y * 10) + centroid$Y + offset_y

    if(any(is.na(n_raw$x))){
      list(
        as.matrix(n_raw[1:(which(is.na(n_raw$x)) - 1),]),
        as.matrix(n_raw[(which(is.na(n_raw$x)) + 1):nrow(n_raw),]))
    }else{
      list(as.matrix(n_raw))
    }
  })

  i      <- 0
  offset <- 5
  for(counter in seq_along(n_raw_mat)){
    # counter <- 1
    char <- n_raw_mat[[counter]]
    # x <- char[[1]]
    n_raw_mat[[counter]] <- lapply(char,function(x){
      x[,1] <- x[,1] + (offset * i)
      x
    })
    i <- i + 1
  }

  n <- lapply(n_raw_mat, function(x) st_sfc(st_polygon(x),
                                            crs = st_crs(bbox)))
  n <- st_geometrycollection(purrr::flatten(n))

  # adjust scale and position (see sf vignette #3)
  ncg   <- st_geometry(n)
  cntrd <- st_centroid(ncg)
  ncg2  <- (ncg - cntrd) * scale_factor + cntrd
  st_sfc(ncg2, crs = st_crs(bbox))
}

# ---- generate base object ----

hu12 <- LAGOSNEgis::query_gis("HU4", "ZoneID", "HU4_49")

# ---- generate hexagonal template ----

hex_template <- spsample(as_Spatial(hu12), type = "hexagonal",
                         cellsize = 20000)
hex_template <- sp::HexPoints2SpatialPolygons(hex_template)
hex_template <- st_as_sf(hex_template)
hex_template <- hex_template[unlist(lapply(
  st_intersects(hex_template, st_centroid(hu12)), function(x) length(x) > 0)),]

# mapview(st_transform(hu12, 4326)) +
#   mapview(st_transform(n, 4326))

# ---- pull_streams ----
lake_streams <- nhd_plus_query(poly = hu12,
                               dsn = c("NHDWaterbody", "NHDFlowLine"),
                               quiet = TRUE)
streams      <- st_transform(lake_streams$sp$NHDFlowLine, st_crs(hu12))
lakes        <- st_transform(lake_streams$sp$NHDWaterbody, st_crs(hu12))

# ---- clip streams to font glyph and scale ----
# http://www.katiejolly.io/blog/2019-01-21/map-cutouts

scale_factor_hex     <- 1
scale_factor_n       <- scale_factor_hex * 880.95

ncg                  <- st_geometry(hex_template)
cntrd                <- st_centroid(ncg)
hex_template_scaled  <- (ncg - cntrd) * scale_factor_hex + cntrd
hex_template_scaled  <- st_sfc(hex_template_scaled, crs = st_crs(hex_template))

n         <- glyph_sf("nhdR", hu12, scale_factor = scale_factor_n, offset_y = -13500)

# n_bbox    <- st_intersection(streams, st_as_sfc(st_bbox(n)))
# n_streams <- st_intersection(n_bbox, n)

# ---- clip streams to hexagon ----

hex_streams <- st_intersection(streams, hex_template_scaled)

# ---- initial plotting ----

gg_logo <- ggplot() +
    geom_sf(data = hex_template_scaled, fill = "lightgreen") +
    geom_sf(data = hex_streams, size = 0.45, color = "blue") +
    # geom_sf(data = n_streams, size = 0.45, color = "cyan") +
    geom_sf(data = n, size = 0.6, fill = "white") +
    coord_sf(datum = NA) +
    cowplot::theme_nothing()
gg_logo

hexSticker::sticker(gg_logo, s_x = 1, s_y = 1, s_width = 2.17, s_height = 2.17,
                    package = "", filename = "logo/logo.png",
                    h_fill = "#ffffff", h_color = "#595959", h_size = 0.8,
                    url = "https://github.com/jsta/nhdR", u_size = 5)

usethis::use_logo("logo/logo.png")
