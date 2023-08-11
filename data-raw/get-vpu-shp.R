library(sf)
library(rgdal)
library(nhdR)

# https://www.epa.gov/waterdata/nhdplus-global-data
remote_path <- "https://https://dmap-data-commons-ow.s3.amazonaws.com/NHDPlusV21/Data/GlobalData/NHDPlusV21_NHDPlusGlobalData_03.7z"
temp_dir <- tempdir()
in_path <- file.path(temp_dir, "nhdglobal.7z")
# in_path <- "/tmp/RtmpsyMbWy/nhdglobal.7z"
# temp_dir <- "/tmp/RtmpsyMbWy"
curl::curl_download(remote_path, in_path)

if (Sys.info()["sysname"] == "Windows") {
  system(paste0("7za.exe e ", in_path, " -o", temp_dir))
} else {
  system(paste0("7z -y e ", in_path, " -o", temp_dir))
}

out_path <- tempfile(fileext = ".shp")
system(paste0("ogr2ogr -simplify 0.06 ", out_path, " ", file.path(temp_dir, "BoundaryUnit.shp")))

vpu_shp <- sf::st_read(out_path)

vpu_shp <- nhdR:::make_valid_geom_s2(vpu_shp)

usethis::use_data(vpu_shp, overwrite = TRUE)
