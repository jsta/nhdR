library(sf)
library(rgdal)

# https://www.epa.gov/waterdata/nhdplus-global-data
remote_path <- "https://s3.amazonaws.com/edap-nhdplus/NHDPlusV21/Data/GlobalData/NHDPlusV21_NHDPlusGlobalData_03.7z"
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

# https://github.com/r-spatial/s2/issues/99#issuecomment-827776431
vpu_s2 <- s2::s2_rebuild(
  s2::as_s2_geography(vpu_shp, check = FALSE),
  options = s2::s2_options(
    edge_type = "undirected", split_crossing_edges = TRUE, validate = TRUE
  )
)
# all(s2::s2_is_valid(vpu_s2))

st_geometry(vpu_shp) <- sf::st_as_sfc(vpu_s2)
# all(s2::s2_is_valid(vpu_shp))

usethis::use_data(vpu_shp, overwrite = TRUE)