# ---- setup ----
library(hexSticker)
library(nhdR)
library(elevatr)
library(HydroData)
library(raster)
library(AOI)
library(sf)
library(ggplot2)
library(dplyr)
library(magick)

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

# ---- remove detail ----
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

# ---- initial plotting ----

gg1 <-  ggplot() +
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

ggsave("gg1.png", gg1)
ggsave("gg2.png", gg2)
ggsave("gg3.png", gg3)

# ---- distort perspective ----
distort_perspective <- function(inpath, outpath){

magick::image_read(inpath) %>%
  magick::image_trim() %>%
  {
    . ->> i_trim
  } %>%
magick::image_write(outpath)
dims   <- magick::image_info(i_trim)[c("width", "height")]

# https://www.imagemagick.org/Usage/distorts/#perspective
# 1st, left to right, width
# 2nd, top to bottom, height

affine_mat <-
paste0(
  # topleft
  paste0("'0,0,", 0.22 * dims[1], ",", 0.66 * dims[2]),
  # topright
  paste0("  ", dims[1], ",0,", 0.77 * dims[1], ",", 0.85  * dims[2]),
  # bottomleft
  paste0("  ", "0,", dims[2], ",", 0.1  * dims[1], ",", 0.85 * dims[2]),
  # bottomright
  paste0("  ", dims[1], ",", dims[2], ",", 0.944 * dims[1], ",", 0.978 * dims[2], "'")
  )
im_str <- paste0("convert ", outpath, " -mattecolor White -virtual-pixel background -background White -distort Perspective ", affine_mat,
                 " ", outpath)
system(im_str)

magick::image_read(outpath) %>%
  magick::image_trim() %>%
  magick::image_write(outpath)
}

distort_perspective("gg1.png", "test1.png")
distort_perspective("gg2.png", "test2.png")
distort_perspective("gg3.png", "test3.png")

image_append(
  c(image_read("test1.png"),
  image_read("test2.png"),
  image_read("test3.png")),
  stack = TRUE
  ) %>%
  image_write("test.png")

# ---- wrapup ----
# manually create logo_gen.png
hexSticker::sticker("logo/logo_gen.png", s_x = 0.9, s_y = 0.84, s_width = 0.74,
                    package = "nhdR", filename = "logo/logo.png", p_size = 20,
                    p_y = 1.5, h_fill = "#ffffff", h_color = "#000000",
                    p_color = "#000000" ,
                    url = "https://github.com/jsta/nhdR")

# usethis::use_logo()
