# nocov start
.onAttach <- function(lib, pkg) {
  if (!has_7z()$yes) {
    packageStartupMessage("The 7-zip program is needed to unpack NHD downloads (http://www.7-zip.org/).")
  }

  # sf::sf_use_s2(FALSE)
}

.onLoad <- function(libname, pkgname) {
  nhd_plus_load <<- memoise::memoise(nhd_plus_load,
    cache = memoise::cache_memory())
}
# nocov end