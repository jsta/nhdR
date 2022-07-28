get_nhdplus_v21_links <- function(){
  has_sub_dirs <- function(raw){
    dir_contents <- stringr::str_extract_all(as.character(raw),
                                             "(?<=\\<Prefix>).*(?=<\\/Prefix>)")
    length(dir_contents[[1]]) > 1
  }

  directory_raw <- read_xml(
    "https://edap-ow-data-commons.s3.amazonaws.com?delimiter=/&prefix=NHDPlusV21/Data/")

  top_directories <- xml2::xml_text(directory_raw)
  top_directories <- strsplit(top_directories, "NHDPlusV21")[[1]]
  top_directories <- top_directories[grep("NHDPlus", top_directories)]
  top_directories <- paste0("NHDPlusV21", top_directories)


  directories <- lapply(top_directories, function(x){
    raw <- read_xml(paste0(
      "https://edap-ow-data-commons.s3.amazonaws.com?delimiter=/&prefix=", x))

    if(has_sub_dirs(raw)){
      res <- data.frame(
        directory = stringr::str_extract_all(as.character(raw),
                                             "(?<=\\<Prefix>).*(?=<\\/Prefix>)")[[1]][-1],
        stringsAsFactors = FALSE)
      res <- mutate(res,
                    vpu = stringr::str_extract(directory,
                                               "\\d{2}\\w{0,1}(?=\\w{0,1}\\/$)"))
    }else{
      vpu <- stringr::str_extract_all(as.character(raw),
                                      "\\d{2}\\w{0,1}(?=\\w{0,1}\\.pdf<\\/Key>)")[[1]]
      if(length(vpu) == 0){
        vpu <- NA
      }
      res <- data.frame(directory = x,
                        vpu = vpu,
                        stringsAsFactors = FALSE)
    }
    # print(x)
    # print(res)
    res
  })

  directories <- dplyr::bind_rows(directories) %>%
    dplyr::filter(!is.na(vpu))

  directories
}

vpu_key <- get_nhdplus_v21_links()

saveRDS(vpu_key, "inst/vpu_key.rds")
