library(wikilake)
library(sf)
library(nhdR)
library(dplyr)
library(magrittr)

# ---- pull_geom ----

# wb <- nhd_plus_load(vpu = 7, dsn = "NHDWaterbody")
# fl <- nhd_plus_load(vpu = 7, dsn = "NHDFlowLine")

sunapee_meta    <- wikilake::lake_wiki("Lake Sunapee")
sunapee         <- st_sfc(st_point(c(sunapee_meta$Lon, sunapee_meta$Lat)))

sunapee <- nhd_plus_query(lon = sunapee_meta$Lon, lat = sunapee_meta$Lat,
                       dsn = c("NHDWaterbody", "NHDFlowLine"), buffer_dist = 0.2)

vpu <- find_vpu(sunapee$pnt)

# ---- pull_sunapee_network ----
sunapee_network <- extract_network(lon = sunapee_meta$Lon, lat = sunapee_meta$Lat,
                                   maxsteps = Inf)

devtools::use_data(sunapee, overwrite = TRUE)
devtools::use_data(sunapee_network, overwrite = TRUE)
