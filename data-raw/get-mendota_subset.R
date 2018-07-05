library(wikilake)
library(sf)
library(nhdR)
library(dplyr)
library(magrittr)

# ---- pull_geom ----

# wb <- nhd_plus_load(vpu = 7, dsn = "NHDWaterbody")
# fl <- nhd_plus_load(vpu = 7, dsn = "NHDFlowLine")

mendota_meta    <- wikilake::lake_wiki("Lake Mendota")
mendota         <- st_sfc(st_point(c(mendota_meta$Lon, mendota_meta$Lat)))

mendota <- nhd_plus_query(lon = mendota_meta$Lon, lat = mendota_meta$Lat,
                       dsn = c("NHDWaterbody", "NHDFlowLine"), buffer_dist = 0.2)

vpu <- find_vpu(mendota$pnt)

# ---- pull_mendota_network ----
mendota_network <- extract_network(lon = mendota_meta$Lon, lat = mendota_meta$Lat,
                                   maxsteps = Inf)

devtools::use_data(mendota, overwrite = TRUE)
devtools::use_data(mendota_network, overwrite = TRUE)
