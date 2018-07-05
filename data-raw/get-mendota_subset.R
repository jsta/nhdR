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
                       dsn = c("NHDWaterbody", "NHDFlowLine"))

vpu <- find_vpu(mendota$pnt)

# ---- pull_flow ----

eromflow  <- nhd_plus_load(vpu, "EROMExtension", "EROM_MA0001") %>%
  filter(ComID %in% mendota$sp$NHDFlowLine$COMID) %>%
  select(ComID, Q0001F)

vogelflow  <- nhd_plus_load(7, "VogelExtension", "vogelflow") %>%
  filter(COMID %in% mendota$sp$NHDFlowLine$COMID,
         MAFLOWV != -9999.00000)

mendota$sp$NHDFlowLine %>%
  left_join(eromflow, by = c("COMID" = "ComID")) %>%
  left_join(vogelflow, by = "COMID") -> mendota_flow

devtools::use_data(mendota, overwrite = TRUE)
devtools::use_data(mendota_flow, overwrite = TRUE)
