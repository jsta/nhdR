library(wikilake)
library(sf)
library(nhdR)
library(dplyr)
library(magrittr)

# ---- pull_geom ----

# wb <- nhd_plus_load(vpu = 4, dsn = "NHDWaterbody")
# fl <- nhd_plus_load(vpu = 4, dsn = "NHDFlowLine")

gull_meta    <- wikilake::lake_wiki("Gull Lake (Michigan)")
gull         <- st_sfc(st_point(c(gull_meta$Lon, gull_meta$Lat)))

gull <- nhd_plus_query(lon = gull_meta$Lon, lat = gull_meta$Lat,
                       dsn = c("NHDWaterbody", "NHDFlowLine"))

# ---- pull_flow ----

eromflow  <- nhd_plus_load(4, "EROMExtension", "EROM_MA0001") %>%
  filter(ComID %in% gull$sp$NHDFlowLine$COMID) %>%
  select(ComID, Q0001F)

vogelflow  <- nhd_plus_load(4, "VogelExtension", "vogelflow") %>%
  filter(COMID %in% gull$sp$NHDFlowLine$COMID,
         MAFLOWV != -9999.00000)

gull$sp$NHDFlowLine %>%
  left_join(eromflow, by = c("COMID" = "ComID")) %>%
  left_join(vogelflow, by = "COMID") -> gull$sp$NHDFlowLine

devtools::use_data(gull, overwrite = TRUE)
