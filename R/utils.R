nhd_path <- function(){
  paste0(rappdirs::user_data_dir(appname = "nhdR",
                                 appauthor = "nhdR"),
                                 .Platform$file.sep)
}

gdb_path <- function(state){
  paste0(nhd_path(), "NHDH_", state, ".gdb")
}

gdb_plus_path <- function(){
  file.path(nhd_path(),
            "NHDPlusV21_NationalData_National_Seamless_Geodatabase_02",
            "NHDPlusNationalData",
            "NHDPlusV21_National_Seamless.gdb")
}

get_if_not_exists <- function(url, destfile){
  if(!file.exists(destfile)){
    curl::curl_download(url, destfile, quiet = TRUE)
  }else{
    message(paste0("A local copy of ", url, " already exists on disk"))
  }
}
