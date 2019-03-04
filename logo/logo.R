library(hexSticker)

hexSticker::sticker("logo/logo_base.png", s_x = 1, s_y = 0.84, s_width = 0.8,
                    package = "nhdR", filename = "logo/logo.png", p_size = 20,
                    p_y = 1.5, h_fill = "#ffffff", h_color = "#000000",
                    p_color = "#000000" ,
                    url = "https://github.com/jsta/nhdR")


library(nhdR)
library(elevatr)
library(HydroData)
library(raster)
library(AOI)
library(sf)
library(ggplot2)
library(dplyr)

data("gull")

# ---- pull huc info ----
if(!file.exists("logo/hucs.rds")){
  hucs       <- HydroData::findWBD(getAOI(st_union(gull$sp$NHDWaterbody)),
                             subbasins = TRUE, crop = FALSE)
  hucs       <- HydroData::findWBD(getAOI(hucs$huc8), subbasins = TRUE)
  hucs$huc8  <- st_as_sf(hucs$huc8)
  hucs$huc10 <- st_as_sf(hucs$huc10)
  hucs$huc12 <- st_as_sf(hucs$huc12)

  saveRDS(hucs, "logo/hucs.rds")
}
hucs <- readRDS("logo/hucs.rds")

huc_8 <- hucs$huc8[which.max(st_area(hucs$huc8)),]
huc_10 <- hucs$huc10[
  which(unlist(lapply(
    st_within(hucs$huc10, huc_8), function(x) length(x) > 0))),]

# ---- pull streams ----
lake_streams <- nhd_plus_query(poly = huc_8,
                               dsn = c("NHDWaterbody", "NHDFlowLine"))
lakes        <- st_transform(lake_streams$sp$NHDWaterbody, st_crs(huc_8))
streams      <- st_transform(lake_streams$sp$NHDFlowLine, st_crs(huc_8))

# ---- pull elevation raster ----
if(!file.exists("logo/elev_raster.tif")){
  elev_raster <- get_elev_raster(as_Spatial(huc_8), z = 8)
  writeRaster(elev_raster, "logo/elev_raster.tif", overwrite = TRUE)
}
elev_raster <- raster::raster("logo/elev_raster.tif")

big_lakes <- lakes %>%
  dplyr::filter(st_area(lakes) > units::as_units(2000000, "m2"))

vpu <- unique(nhdR::find_vpu(streams))

big_streams <- nhd_plus_load(vpu, "EROMExtension", "EROM_MA0001") %>%
  dplyr::filter(ComID %in% streams$COMID) %>%
  dplyr::select(ComID, Q0001F) %>%
  right_join(streams, by = c("ComID" = "COMID")) %>%
  dplyr::filter(Q0001F > 50)

big_raster <- elev_raster %>%
  raster::crop(huc_8) %>%
  raster::mask(huc_8) %>%
  as.data.frame(xy = TRUE)

gg1<-  ggplot() +
    geom_sf(data = huc_8, size = 1.2) +
    geom_sf(data = big_streams, color = "cyan") +
    geom_sf(data = big_lakes, fill = "cyan", color = "blue") +
    coord_sf(datum = NA) +
    cowplot::theme_nothing()

gg2 <- ggplot() +
    geom_sf(data = huc_8, size = 1.2) +
    geom_sf(data = huc_10, size = 0.5) +
    coord_sf(datum = NA) +
    cowplot::theme_nothing()

gg3 <- ggplot() +
    geom_raster(data = big_raster, aes(x = x, y = y, fill = elev_raster)) +
    geom_sf(data = huc_8, size = 1.2, alpha = 0) +
    scale_fill_viridis_c(na.value = "white") +
    coord_sf(datum = NA) +
    cowplot::theme_nothing()

# ---- distort perspective ----
library(magick)

ggsave("gg1.png", gg1)

outpath <- "test.png"
finalpath <- "test2.png"

i_trim <- magick::image_read("gg1.png") %>%
  magick::image_trim()
dims   <- magick::image_info(i_trim)[c("width", "height")]

magick::image_write(i_trim, outpath)

# https://www.imagemagick.org/Usage/distorts/#perspective
# 1st, left to right, width: 1869
# 2nd, top to bottom, height: 1671
# topleft topright bottomleft bottomright

affine_mat <-
  "'0,0,415,1114  1869,0,1453,1169  0,1671,103,1170  1869,1671,1765,1634'"

affine_mat <-
paste0(
  paste0("'0,0,", 0.22 * dims[1], ",", 0.66 * dims[2]),
  paste0(" ", dims[1], ",0,", 0.77 * dims[1], ",", 0.7  * dims[2]),
  paste0(" ", "0,", dims[2], ",", 0.055  * dims[1], ",", 0.7 * dims[2]),
  paste0(" ", dims[1], ",", dims[2], ",", 0.944 * dims[1], ",", 0.978 * dims[2], "'")
  )
im_str <- paste0("convert ", outpath, " -distort Perspective ", affine_mat,
                 " ", finalpath)

system(im_str)


# convert gg2.png -distort Perspective '0,0,415,1114  1869,0,1453,1169  0,1671,103,1170  1869,1671,1765,1634' checks_horizon.png
#
# '0,0,0,0  0,1671,0,1671  1869,0,1896,464  1869,1671,1869,1206'
#
# '0,0,415,1114  1869,0,1453,1169  0,1671,103,1170  1869,1671,1765,1634'
#
# topleft        topright		bottomleft	 bottomright
# '0,0,200,1114  1669,0,1869,1114  0,1671,0,1634  1869,1671,1869,1634'
#
# 1st, left to right, width: 1869
# 2nd, top to bottom, height: 1671





# usethis::use_logo()
