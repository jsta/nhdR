
library(LAGOSNEgis)
library(fontr)
library(sf)
library(mapview)
library(nhdR)
library(ggplot2)
library(cowplot)
library(hexSticker)
library(sp)

hu12 <- LAGOSNEgis::query_gis("HU4", "ZoneID", "HU4_49")

# generate hexagonal template ----

hex_template <- spsample(as_Spatial(hu12), type = "hexagonal",
                         cellsize = 20000)
hex_template <- sp::HexPoints2SpatialPolygons(test)
hex_template <- st_as_sf(hex_template)
hex_template <- hex_template[unlist(lapply(
  st_intersects(hex_template, st_centroid(hu12)), function(x) length(x) > 0)),]


#  ---- intersect text glyph with watershed ----
# https://djnavarro.net/post/in-between.html

glyph_sf <- function(chars, bbox, scale_factor = 3700){
  # chars <- "nhdR"
  # bbox <- hu12
  n_raw_mat <- lapply(strsplit(chars, "")[[1]], function(x){
    n_raw       <- fontr::glyph_polygon(x)
    centroid    <- st_centroid(st_as_sfc(st_bbox(bbox)))
    centroid    <- as.data.frame(st_coordinates(centroid))
    # multiplication avoids rounding errors
    n_raw$x     <- (n_raw$x * 10) + centroid$X
    n_raw$y     <- (n_raw$y * 10) + centroid$Y

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

  n_raw_mat <- purrr::flatten(n_raw_mat)
  n           <- st_sfc(st_polygon(n_raw_mat), crs = st_crs(bbox))

  # adjust scale and position (see sf vignette #3)
  ncg   <- st_geometry(n)
  cntrd <- st_centroid(ncg)
  ncg2  <- (ncg - cntrd) * scale_factor + cntrd
  st_sfc(ncg2, crs = st_crs(bbox))
}

# mapview(st_transform(hu12, 4326)) +
#   mapview(st_transform(n, 4326))

# ---- pull_streams ----
lake_streams <- nhd_plus_query(poly = hu12,
                               dsn = c("NHDWaterbody", "NHDFlowLine"),
                               quiet = TRUE)
streams      <- st_transform(lake_streams$sp$NHDFlowLine, st_crs(hu12))
lakes        <- st_transform(lake_streams$sp$NHDWaterbody, st_crs(hu12))

# ---- clip streams to font glyph ----
# http://www.katiejolly.io/blog/2019-01-21/map-cutouts

n         <- glyph_sf("n", hu12)
n_streams <- st_intersection(streams, n)
h         <- glyph_sf("h", hu12)
h_streams <- st_intersection(streams, h)
d         <- glyph_sf("d", hu12)
d_streams <- st_intersection(streams, d)
R         <- glyph_sf("R", hu12)
R_streams <- st_intersection(streams, R)

# ---- clip streams to hexagon ----

hex_streams <- st_intersection(streams, hex_template)

# ---- initial plotting ----

gg_logo <- function(n, n_streams){
  ggplot() +
    geom_sf(data = n_streams, size = 0.3, color = "blue") +
    geom_sf(data = n, size = 0.6, alpha = 0) +
    coord_sf(datum = NA) +
    cowplot::theme_nothing()
}

pl <- list(gg_logo(n, n_streams) + theme(plot.margin = margin(0, 0, 0, 0)),
           gg_logo(h, h_streams),
           gg_logo(d, d_streams),
           gg_logo(R, R_streams)
)

pkg_name <- cowplot::plot_grid(plotlist = pl, nrow = 1,
                   align = "h",
                   axis = "b",
                   scale = c(1, 1, 1, 1))
                   # scale = c(0.65, 0.7, 0.7, 1))

hexSticker::sticker(pkg_name, s_x = 1, s_y = 1, s_width = 1.7, s_height = 0.7,
                    package = "", filename = "logo/logo.png",
                    h_fill = "#ffffff", h_color = "#595959", h_size = 0.8,
                    url = "https://github.com/jsta/nhdR", u_size = 5)

usethis::use_logo("logo/logo.png")
