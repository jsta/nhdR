
library(LAGOSNEgis)
library(fontr)
library(sf)
library(mapview)

#  ---- intersect text glyph with watershed ----
# https://djnavarro.net/post/in-between.html

hu12 <- LAGOSNEgis::query_gis("HU4", "ZoneID", "HU4_49")

n_raw       <- fontr::glyph_polygon("n")
centroid    <- st_centroid(st_as_sfc(st_bbox(hu12)))
centroid    <- as.data.frame(st_coordinates(centroid))
n_raw$x     <- (n_raw$x * 10) + centroid$X
n_raw$y     <- (n_raw$y * 10) + centroid$Y
n           <- st_sfc(st_polygon(list(as.matrix(n_raw))), crs = st_crs(hu12))

# adjust scale and position (see sf vignette #3)
ncg   <- st_geometry(n)
cntrd <- st_centroid(ncg)

ncg2  <- (ncg - cntrd) * 3700 + cntrd
ncg2  <- st_sfc(ncg2, crs = st_crs(hu12))

mapview(st_transform(hu12, 4326)) +
  mapview(st_transform(ncg2, 4326))

# ---- pull_streams ----
lake_streams <- nhd_plus_query(poly = hu12,
                               dsn = c("NHDWaterbody", "NHDFlowLine"),
                               quiet = TRUE)
streams      <- st_transform(lake_streams$sp$NHDFlowLine, st_crs(hu12))
lakes        <- st_transform(lake_streams$sp$NHDWaterbody, st_crs(hu12))

# ---- clip streams to font glyph ----
# http://www.katiejolly.io/blog/2019-01-21/map-cutouts

test <- st_intersection(streams, ncg2)
mapview(test)

# ---- initial plotting ----

gg1 <-  ggplot() +
    geom_sf(data = huc_8, size = 1.2) +
    geom_sf(data = big_streams, color = "cyan") +
    geom_sf(data = big_lakes, fill = "cyan", color = "blue") +
    coord_sf(datum = NA) +
    cowplot::theme_nothing()

ggsave("gg1.png", gg1)
