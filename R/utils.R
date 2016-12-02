nhd_path <- function(){
  paste0(rappdirs::user_data_dir(appname = "nhdR",
                                 appauthor = "nhdR"),
                                 .Platform$file.sep)
}

gdb_path <- function(){
  file.path(nhd_path(),
            "NHDPlusV21_NationalData_National_Seamless_Geodatabase_02",
            "NHDPlusNationalData",
            "NHDPlusV21_National_Seamless.gdb")
}
