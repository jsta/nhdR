#' nhd_get
#' @param state character state abbrevation includes "DC", "PR", and "VI"
#' @export
#' @importFrom utils unzip
#' @import maps
#' @examples \dontrun{
#' nhd_get(state = c("DC", "HI"))
#' }
nhd_get <- function(state = NA){

  baseurl <- "ftp://nhdftp.usgs.gov/DataSets/Staged/States/FileGDB/HighResolution/"

  nhd_get_state <- function(state){

    if(!state %in% c(as.character(maps::state.fips$abb), "DC", "PR", "VI", "HI")){
      stop(paste0(state, " is not a valid state abbreviation"))
    }

    filename <- paste0("NHDH_", state, "_931v220.zip")
    url      <- paste0(baseurl, filename)
    destfile <- paste0(nhd_path(), filename)

    get_if_not_exists(url, destfile)
    unzip(destfile, exdir = nhd_path())
  }

  invisible(lapply(state, nhd_get_state))

}

