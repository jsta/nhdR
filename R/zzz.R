# nocov start
.onAttach <- function(lib, pkg){
  if(!has_7z()$yes){
    packageStartupMessage("The 7-zip program is needed to unpack NHD downloads (http://www.7-zip.org/).")
  }
}
# nocov end
