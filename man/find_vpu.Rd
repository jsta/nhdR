% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{find_vpu}
\alias{find_vpu}
\title{Find VPU}
\usage{
find_vpu(pnt)
}
\arguments{
\item{pnt}{sf object}
}
\value{
A character vector of vpu ids
}
\description{
Find Vector Processing Unit from sf object
}
\examples{
\dontrun{
library(sf)

# vpu centers
pnt <- st_cast(st_point_on_surface(nhdR::vpu_shp), "POINT")

find_vpu(pnt[1, ])
find_vpu(pnt)

find_vpu(nhdR::gull$sp$NHDWaterbody[1, ])
find_vpu(nhdR::gull$sp$NHDWaterbody)
}
}
