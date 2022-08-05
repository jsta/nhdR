#' Download and cache NHD data by state
#'
#' @param state character state abbreviation includes "DC", "PR", and "VI"
#' @inheritParams nhd_plus_get
#' @return An invisible list of file paths to NHD data for the specified state
#' @export
#' @importFrom utils unzip
#' @import maps
#' @examples \dontrun{
#' nhd_get(state = c("DC"))
#' nhd_get(state = c("RI", "CT"))
#' }
nhd_get <- function(state = NA, force_dl = FALSE, force_unzip = FALSE, temporary = TRUE) {

  baseurl <- paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/State/")

  nhd_get_state <- function(state) {

    if (!state %in% c(as.character(maps::state.fips$abb),
      "DC", "PR", "VI", "HI")) {
      stop(paste0(state, " is not a valid state abbreviation"))
    }

    # state <- "DC"
    state_name <- stateabb2name(state)
    remotepath <- get_remotepath(state_name, baseurl)
    url        <- remotepath$url
    filename   <- remotepath$filename
    destfile   <- file.path(nhd_path(), filename)

    get_if_not_exists(url, destfile, force_dl = force_dl)
    unzip(destfile, exdir = nhd_path(temporary))

    # if(is_gpkg_installed()){
    #   compile_gpkg(state)
    # }
  }

  invisible(lapply(state, nhd_get_state))
}

#' Download and cache NHDplus data by vector processing unit
#'
#' @param vpu numeric vector processing unit
#' @param component character component name
#' @param force_dl logical force a re-download of the requested data
#' @param force_unzip logical force an unzip of downloaded data
#' @param temporary logical set FALSE to save data to a persistent
#'  rappdirs location
#' @return An invisible list of file paths to NHDplus data for the specified vpu
#' @export
#' @importFrom utils unzip
#' @importFrom rvest html_nodes html_attrs
#' @importFrom xml2 read_html
#' @examples \dontrun{
#' # Spatial
#' nhd_plus_get(vpu = 4)
#' nhd_plus_get(vpu = "10L")
#' nhd_plus_get(vpu = 1, component = "NHDPlusAttributes")
#'
#' # Non-spatial
#' nhd_plus_get(vpu = "National", component = "V1_To_V2_Crosswalk")
#' nhd_plus_get(vpu = 4, component = "EROMExtension")
#' }
nhd_plus_get <- function(vpu = NA, component = "NHDSnapshot", force_dl = FALSE,
                         force_unzip = FALSE, temporary = TRUE) {

  if (!curl::has_internet()) {
    stop("This function requires internet access.")
  }

  if (!(component %in% c("NHDSnapshot", "NHDPlusCatchment",
    "NHDPlusAttributes", "V1_To_V2_Crosswalk",
    "EROMExtension", "VogelExtension"))) {
    stop(paste0("Component '", component,
      "' was not found. Is it misspelled?"))
  }

  if (!vpu %in% c("National", 1:2, 4:9, 11:22, "10L", "10U", "03N", "03W", "03S",
    paste0("0", 1:2), paste0("0", 4:9),
    as.character(nhdR::vpu_shp$UnitID))) {
    stop(paste0(vpu, " is not a valid vpu. Are you missing a letter designation? See VPU map."))
  }

  url <- get_plus_remotepath(vpu, component = component)

  destdir <- file.path(nhd_path(temporary), "NHDPlus")
  destsubdir <- file.path(destdir, paste(
    strsplit(basename(url), "_")[[1]][2:4], collapse = "_"))

  dir.create(destdir, showWarnings = FALSE)
  dir.create(destsubdir, showWarnings = FALSE)
  destfile <- file.path(destdir, basename(url))

  if (force_unzip & !force_dl) {
    if (!has_7z()$yes) {
      stop("The 7-zip program is needed to unpack NHD downloads (http://www.7-zip.org/).")
    }
    system(paste0(has_7z()$path, " e -y ", shQuote(normalizePath(destfile)),
      " -o", shQuote(normalizePath(destsubdir))))
  } else {
    if (get_if_not_exists(url, destfile, force_dl = force_dl)) {
      if (!has_7z()$yes) {
        stop("The 7-zip program is needed to unpack NHD downloads (http://www.7-zip.org/).")
      }
      system(paste0(has_7z()$path, " e ", shQuote(normalizePath(destfile)),
        " -o", shQuote(normalizePath(destsubdir))))
    }
  }
}