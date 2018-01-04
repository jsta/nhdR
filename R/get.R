#' Download and cache NHD data by state
#'
#' @param state character state abbrevation includes "DC", "PR", and "VI"
#' @export
#' @importFrom utils unzip
#' @import maps
#' @examples \dontrun{
#' nhd_get(state = c("DC"))
#' nhd_get(state = c("RI", "CT"))
#' }
nhd_get <- function(state = NA){

  baseurl <- paste0("ftp://nhdftp.usgs.gov/DataSets/Staged/",
                    "States/FileGDB/HighResolution/")

  nhd_get_state <- function(state){

    if(!state %in% c(as.character(maps::state.fips$abb),
                     "DC", "PR", "VI", "HI")){
      stop(paste0(state, " is not a valid state abbreviation"))
    }

    filename <- paste0("NHDH_", state, "_931v220.zip")
    url      <- paste0(baseurl, filename)
    destfile <- file.path(nhd_path(), filename)

    get_if_not_exists(url, destfile)

    unzip(destfile, exdir = nhd_path())

    if(is_gpkg_installed()){
      compile_gpkg(state)
    }
  }

  invisible(lapply(state, nhd_get_state))
}

#' Download and cache NHDplus data by state
#'
#' @param vpu numeric vector processing unit
#' @param component character component name
#' @export
#' @importFrom utils unzip
#' @importFrom rvest html_nodes html_attrs
#' @importFrom xml2 read_html
#' @examples \dontrun{
#' nhd_plus_get(vpu = 4)
#' nhd_plus_get(vpu = "10L")
#' nhd_plus_get(vpu = 1, component = "NHDPlusAttributes")
#'
#' nhd_plus_get(vpu = "National", component = "V1_To_V2_Crosswalk")
#' }
nhd_plus_get <- function(vpu = NA, component = "NHDSnapshot"){

  if(!curl::has_internet()){
    stop("This function requires internet access.")
  }

  if(!(component %in% c("NHDSnapshot", "NHDPlusCatchment",
                        "NHDPlusAttributes", "V1_To_V2_Crosswalk"))){
    stop(paste0("Component '", component,
                "' was not found. Is it misspelled?"))
  }

  if(!vpu %in% c("National", 1:22, "10L", "10U", "03N", "03W", "03S",
                 paste0("0", 1:9), as.character(nhdR::vpu_shp$UnitID))){
    stop(paste0(vpu, " is not a valid vpu"))
  }

  url <- get_plus_remotepath(vpu, component = component)

  destdir <- file.path(nhd_path(), "NHDPlus")
  destsubdir <- file.path(destdir, paste(
                  strsplit(basename(url), "_")[[1]][2:4], collapse = "_"))
  dir.create(destdir, showWarnings = FALSE)
  dir.create(destsubdir, showWarnings = FALSE)
  destfile <- file.path(destdir, basename(url))

  if(get_if_not_exists(url, destfile)){
    if(Sys.info()["sysname"] == "Windows"){
      system(paste0("7za.exe e ", destfile, " -o", destsubdir))
    }else{
      system(paste0("7z e ", destfile, " -o", destsubdir))
    }
  }
}

