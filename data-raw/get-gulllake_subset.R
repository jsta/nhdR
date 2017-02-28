library(wikilake)
library(sf)
library(nhdR)

wb <- nhd_plus_load(vpu = 4, "NHDWaterbody")
fl <- nhd_plus_load(vpu = 4, component = "NHDFlowLine")

gull_meta <- wikilake::lake_wiki("Gull Lake (Michigan)")
gull <- st_sfc(st_point(c(gull_meta$Lon, gull_meta$Lat)))
gull_buff <- st_sfc(st_buffer(gull, dist = 0.05))
st_crs(gull) <- st_crs(gull_buff) <- 4326 #wgs84 epsg
st_crs(wb) <- st_crs(fl) <- 4269

gull      <- st_transform(gull, crs = "+proj=utm +zone=10 +datum=WGS84")
gull_buff <- st_transform(gull_buff, crs = "+proj=utm +zone=10 +datum=WGS84")

wb        <- st_transform(wb, crs = "+proj=utm +zone=10 +datum=WGS84")
fl        <- st_transform(fl, crs = "+proj=utm +zone=10 +datum=WGS84")

wb_intersecting <- unlist(lapply(st_intersects(wb, gull_buff), length)) > 0
wb_sub <- wb[wb_intersecting,]

fl_intersecting <- unlist(lapply(st_intersects(fl, gull_buff), length)) > 0
fl_sub <- fl[fl_intersecting,]

gull <- list(wb_sub = wb_sub,
            fl_sub = fl_sub)

devtools::use_data(gull)
