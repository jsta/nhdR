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
    message(paste0("Downloading ", url))
    suppressWarnings(
      httr::GET(url, httr::write_disk(destfile), httr::progress()))
    TRUE
  }else{
    message(paste0("A local copy of ", url, " already exists on disk"))
    FALSE
  }
}

zero_pad <- function(x, digits){
  paste0(paste0(rep(0, digits), collapse = ""), x, collapse = "")
}

get_plus_remotepath <- function(vpu, component = "NHDSnapshot"){
  baseurl <- paste0("http://www.horizon-systems.com/nhdplus/NHDPlusV2_",
                    zero_pad(vpu, 1), ".php")
  res <- rvest::html_attrs(rvest::html_nodes(xml2::read_html(baseurl), "a"))
  res <- unlist(res[grep(component, res)])
  res <- res[!(1:length(res) %in% grep("FGDB", res))][1]
  res
}

is_spatial <- function(filename){
  length(grep("shp$", filename)) > 0
}
