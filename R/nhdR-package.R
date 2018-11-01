#' R interface to the National Hydrography Dataset
#' @name nhdR-package
#' @aliases nhdR
#' @docType package
#' @importFrom httr GET write_disk progress
#' @importFrom ggplot2 map_data
#' @importFrom sf st_drivers
#' @title R interface to the National Hydrography Dataset
#' @author \email{stachel2@msu.edu}
NULL

#' vpu_shp
#'
#' @title Low-res simple features data frame of the NHDPlus vector processing
#' units
#'
#' @docType data
#' @keywords datasets
#' @name vpu_shp
NULL

#' gull
#'
#' @title List of simple features lake polygons and flowlines within a buffer
#' around Gull Lake Michigan.
#' @description Data from NHD Plus
#' @docType data
#' @keywords datasets
#' @name gull
NULL

#' gull_flow
#'
#' @title Flowlines within a buffer around Gull Lake Michigan including flow information.
#' @description Data from NHD Plus
#' @docType data
#' @keywords datasets
#' @name gull_flow
NULL

#' mendota
#'
#' @title List of simple features lake polygons and flowlines within a buffer
#' around Lake Mendota.
#' @description Data from NHD Plus
#' @docType data
#' @keywords datasets
#' @name mendota
NULL

#' mendota_network
#'
#' @title Upstream flowlines connected to Lake Mendota.
#' @description Data from NHD Plus
#' @docType data
#' @keywords datasets
#' @name mendota_network
NULL

#' sunappe
#'
#' @title List of simple features lake polygons and flowlines within a buffer
#' around Lake Sunappe.
#' @description Data from NHD Plus
#' @docType data
#' @keywords datasets
#' @name sunappe
NULL

#' sunappe_network
#'
#' @title Upstream flowlines connected to Lake Sunappe.
#' @description Data from NHD Plus
#' @docType data
#' @keywords datasets
#' @name sunappe_network
NULL
