nhd_path <- function(temporary = TRUE) {
  if (nchar(Sys.getenv("nhdR_path")) != 0) {
    path <- Sys.getenv("nhdR_path")
    return(path)
  }

  if (temporary) {
    warning(
      "Recommended: set the 'temporary' argument to FALSE to save data to a
      persistent rappdirs location.")
    path <- tempdir()
    Sys.setenv(nhdR_path = path)
    return(path)
  } else {
    path <- file.path(rappdirs::user_data_dir(appname = "nhdR",
      appauthor = "nhdR"))
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    Sys.setenv(nhdR_path = path)
    return(path)
  }
}

gdb_path <- function(state) {
  paste0(nhd_path(), .Platform$file.sep, "NHD_H_", stateabb2name(state), "_State_GDB.gdb")
}

gdb_plus_path <- function() {
  file.path(nhd_path(),
    "NHDPlusV21_NationalData_National_Seamless_Geodatabase_02",
    "NHDPlusNationalData",
    "NHDPlusV21_National_Seamless.gdb")
}

# zip_plus_path <- function(){
#   file.path
#
# }

get_if_not_exists <- function(url, destfile, force_dl = FALSE) {
  if (force_dl) {
    message(paste0("Re-downloading ", url))
    suppressWarnings(
      httr::GET(url, httr::write_disk(destfile, overwrite = TRUE), httr::progress()))
    TRUE
  }

  if (!file.exists(destfile)) {
    message(paste0("Downloading ", url))
    suppressWarnings(
      httr::GET(url, httr::write_disk(destfile), httr::progress()))
    TRUE
  } else {
    message(paste0("A local copy of ", url, " already exists on disk"))
    FALSE
  }
}

zero_pad <- function(x, digits) {
  if (nchar(stringr::str_extract(x, "\\d+")) < 2) {
    paste0(
      paste0(
        rep(0, digits), collapse = ""), x, collapse = "")
  } else {
    x
  }
}

get_remotepath <- function(state, baseurl) {
  # state  <- "Missouri"
  # baseurl <- paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/State/HighResolution/")
  filename <- paste0("NHD_H_", state, "_State_GDB.zip")
  url      <- paste0(baseurl, "GDB/", filename)
  list(filename = filename, url = url)
}

# get_plus_remotepath("9")
#' @importFrom xml2 read_xml
get_plus_remotepath <- function(vpu, component = "NHDSnapshot") {

  if (vpu == "National") {
    prefix <- "NHDPlusV21/Data/NationalData/"
  } else {
    vpu_key <- readRDS(system.file("vpu_key.rds", package = "nhdR"))
    prefix <- vpu_key[vpu_key$vpu == zero_pad(vpu, 1), "directory"]
  }

  baseurl <- paste0(
    "https://s3.amazonaws.com/edap-nhdplus?delimiter=/&prefix=",
    prefix)
  res <- read_xml(baseurl)
  res <- stringr::str_extract_all(as.character(res),
    "(?<=\\<Key>).*(?=<\\/Key>)")[[1]]

  if (vpu == "National") {
    res <- unlist(res[grep(component, res)])
  } else {
    res <- unlist(res[grep(paste0(vpu, "_", component), res)])
  }

  res <- res[!(seq_len(length(res)) %in%
    c(grep("FGDB", res), grep(".pdf", res), grep("FileGDB", res)))]

  paste0("https://s3.amazonaws.com/edap-nhdplus/", res[1])
}

is_spatial <- function(filename) {
  length(grep("shp$", filename)) > 0
}

#' Find VPU
#'
#' Find Vector Processing Unit from sf object
#'
#' @param pnt sf object
#'
#' @importFrom sf st_transform st_crs st_join st_distance
#' @importFrom dplyr select
#' @export
#' @return A character vector of vpu ids
#'
#' @examples \dontrun{
#' library(sf)
#'
#' # vpu centers
#' pnt <- st_cast(st_point_on_surface(nhdR::vpu_shp), "POINT")
#'
#' find_vpu(pnt[1, ])
#' find_vpu(pnt)
#'
#' find_vpu(nhdR::gull$sp$NHDWaterbody[1, ])
#' find_vpu(nhdR::gull$sp$NHDWaterbody)
#' }
find_vpu <- function(pnt) {
  # fix for older proj versions (solaris)
  # https://stackoverflow.com/a/62268361/3362993
  vpu <- nhdR::vpu_shp
  sf::st_crs(vpu$geometry) <- 4326

  pnt <- sf::st_transform(pnt, sf::st_crs(vpu))
  vpu <- vpu[vpu$UnitType == "VPU", ]

  if (any(names(pnt) == "UnitID")) {
    pnt <- pnt[, !(names(pnt) %in% "UnitID")]
  }

  res <- suppressMessages(st_join(sf::st_sf(pnt), vpu)$UnitID)

  if (all(is.na(res))) { # pnt is slightly outside of the vpu extent
    res <- vpu[which.min(sf::st_distance(vpu, pnt)), ]$UnitID
  }

  as.character(res)
}

find_state <- function(pnt) {
  state_data_sf <- sf::st_as_sf(map("state", plot = FALSE, fill = TRUE))
  res <- sf::st_transform(state_data_sf, sf::st_crs(pnt))
  res <- make_valid_geom_s2(res)

  res_intersects <- sf::st_intersects(res, pnt)

  state <- res$ID[
    which(unlist(lapply(res_intersects, length)) > 0)]
  state
}

nhd_read_dbf <- function(state, dsn) {
  temp_dir <- tempdir()
  # sf::gdal_utils("info", normalizePath(gdb_path(state)))
  suppressWarnings(sf::gdal_utils("vectortranslate",
    normalizePath(gdb_path(state)),
    temp_dir,
    options = c("-overwrite")
  ))
  read.dbf(file.path(temp_dir, paste0(dsn, ".dbf")))
}

# https://stackoverflow.com/a/9188972/3362993
long2UTM <- function(long) {
  (floor((long + 180) / 6) %% 60) + 1
}

#' Re-project to appropriate UTM zone
#'
#' @param sf_object an sf object
#'
#' @importFrom sf st_transform st_crs
#' @export
#' @return A transformed sf object
#'
#' @examples \dontrun{
#' data(gull)
#' gull_ <- gull$sp$NHDWaterbody
#' st_crs(gull_)
#' gull_ <- st_transform(gull_, 4326)
#' st_crs(gull_)
#' st_crs(toUTM(gull_[1, ]))
#' }
toUTM <- function(sf_object) {

  if (is.na(st_crs(sf_object)$epsg)) {
    sf_object <- st_transform(sf_object, crs = 4326)
  }

  if (sf::st_is_longlat(sf_object)) {
    utm_zone <- suppressWarnings(long2UTM(st_coordinates(st_centroid(st_union(sf_object)))[1]))
    crs      <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84")

    sf::st_transform(sf_object, crs = crs)
  } else {
    sf_object
  }
}

compile_gpkg <- function(state) {
  # state = "DC"
  dest <- suppressWarnings(
    normalizePath(gsub(".gdb", ".gpkg", gdb_path(state)))
  )
  # file.exists(dest)
  sf::gdal_utils("vectortranslate",
    source = normalizePath(gdb_path(state)),
    destination = dest,
    options = c("-f", "GPKG")
  )
}

is_gpkg_installed <- function() {
  name <- rlang::quo("name")
  all(as.logical(dplyr::filter(sf::st_drivers(), name == "GPKG")[3:6]))
}

#' Convert a bounding box to polygon
#'
#' @param bbox object of class bbox from sf
#'
#' @importFrom sf st_as_sfc
#' @export
#' @return An sfc object from the sf package
#' @examples \dontrun{
#' library(sf)
#' wk <- wikilake::lake_wiki("Gull Lake (Michigan)")
#'
#' pnt <- st_as_sf(wk, coords = c("Lon", "Lat"), crs = 4326)
#' pnt <- st_transform(pnt, st_crs(vpu_shp))
#' qry <- nhd_plus_query(wk$Lon, wk$Lat,
#'   dsn = c("NHDWaterbody"), buffer_dist = 0.05)
#' wbd <- qry$sp$NHDWaterbody[which.max(st_area(qry$sp$NHDWaterbody)), ]
#' bbox2poly(st_bbox(wbd))
#' }
bbox2poly <- function(bbox) {
  sf::st_as_sfc(bbox)
}

# https://stackoverflow.com/a/14965990/3362993
has_7z <- function() {
  paths_7z <- c("7z",
    path.expand("~/usr/bin/7z"),
    "C:\\PROGRA~1\\7-Zip\\7za",
    "C:\\PROGRA~1\\7-Zip\\7z.exe")
  if (!any(nchar(Sys.which(paths_7z)) > 0)) {
    list(yes = FALSE, path = NA)
  } else {
    list(yes = TRUE, path = paths_7z[nchar(Sys.which(paths_7z)) > 0][1])
  }
}

get_utm_zone <- function(crs) {
  crs <- as.character(crs$proj4string)
  stringr::str_extract(crs, "(?!\\+zone=)(\\d+)(?=\\s\\+datum)")
}

stateabb2name <- function(abb) {
  # state <- "DC"
  key <- data.frame(sabb = c(datasets::state.abb, "DC"),
    sname =
      gsub(" ", "_",
        c(datasets::state.name, "District of Columbia")),
    stringsAsFactors = FALSE)

  res <- dplyr::filter(key, .data$sabb == abb)
  res$sname
}

albers_conic <- function() {
  # Albers Equal Area Conic
  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
}

#' Data and spatial polygons of the Great Lakes
#'
#' @importFrom purrr transpose
#' @importFrom sf st_as_sf
#' @export
#' @param spatial logical, return Great Lakes polygons?
#' @return A data frame of North America Great Lakes with
#'  optional geometry column
#' @examples
#' gl <- great_lakes()
#' \dontrun{
#' gl <- great_lakes(spatial = TRUE)
#' }
great_lakes <- function(spatial = FALSE) {
  res <- data.frame(
    GNIS_NAME = c("Lake Michigan", "Lake Erie", "Lake Huron", "Lake Ontario",
      "Lake Superior"),
    lon_dd = c(-87.0, -81.2, -82.4, -77.9, -87.5),
    lat_dd = c(44.0, 42.2, 44.8, 43.7, 47.0),
    stringsAsFactors = FALSE)

  if (spatial) {
    sp <- lapply(purrr::transpose(res), function(x) {
      res <- nhd_plus_query(
        x$lon_dd,
        x$lat_dd,
        dsn = "NHDWaterbody", buffer_dist = 0.3)$sp$NHDWaterbody

      if (nrow(res) > 0) {
        res <- st_transform(res, albers_conic())
      }
    })
    sp  <- do.call(what = rbind, args = sp)
    res <- st_as_sf(dplyr::left_join(res, sp, by = "GNIS_NAME"))
  }

  res
}

#' @importFrom utils capture.output
st_read_custom <- function(x, pretty = FALSE, wkt_filter = NA, ...) {
  if (isTRUE(pretty)) {
    msg <- capture.output(res <- sf::st_read(x, ...))
    if (length(msg) > 0) {
      msg <-
        stringr::str_extract(msg[1],
          "([?:`].*)(?=' from data source)")
      msg <- substring(msg, 2, nchar(msg))
      message(paste0("Reading layer '", msg, "'"))
      res
    } else {
      res
    }
  } else {
    if (!is.na(wkt_filter)) {
      sf::st_read(x, wkt_filter = wkt_filter, ...)
    } else {
      sf::st_read(x, ...)
      # do.call(sf::st_read, c("dsn" = x, arguments))
    }
  }
}

align_names <- function(to, from) {
  res <- rep(NA, length(to))
  for (i in seq_along(to)) {
    if (tolower(to[i]) %in% tolower(from)) {
      res[i] <- from[which(tolower(to[i]) == tolower(from))]
    } else {
      res[i] <- to[i]
    }
  }
  res
}

# https://github.com/r-spatial/s2/issues/99#issuecomment-827776431
make_valid_geom_s2 <- function(sf_object) {
  sf_s2 <- s2::s2_rebuild(
    s2::as_s2_geography(sf_object, check = FALSE),
    options = s2::s2_options(
      edge_type = "undirected", split_crossing_edges = TRUE, validate = TRUE
    )
  )
  # all(s2::s2_is_valid(vpu_s2))

  sf::st_geometry(sf_object) <- sf::st_as_sfc(sf_s2)
  sf_object
}