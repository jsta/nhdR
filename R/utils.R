nhd_path <- function(){
  path <- file.path(rappdirs::user_data_dir(appname = "nhdR",
                                      appauthor = "nhdR"))
  dir.create(path, showWarnings = FALSE)
  path
}

gdb_path <- function(state){
  paste0(nhd_path(), .Platform$file.sep, "NHDH_", state, ".gdb")
}

gdb_plus_path <- function(){
  file.path(nhd_path(),
            "NHDPlusV21_NationalData_National_Seamless_Geodatabase_02",
            "NHDPlusNationalData",
            "NHDPlusV21_National_Seamless.gdb")
}

# zip_plus_path <- function(){
#   file.path
#
# }

get_if_not_exists <- function(url, destfile){
  if(!file.exists(destfile)){
    curl::curl_download(url, destfile, quiet = TRUE)
    TRUE
  }else{
    message(paste0("A local copy of ", url, " already exists on disk"))
    FALSE
  }
}

zero_pad <- function(x, digits){
  paste0(paste0(rep(0, digits), collapse = ""), x, collapse = "")
}
