#' Example shapefile
#'
#' An example boundary shapefile containing the boundary of Kapiri Mposhi district in Zambia.
#'
#' @format ## `example_shp`
#' A SpatialPolygons object with no features:
#'
#' @source <https://malariaatlas.org/malariaatlas-r-package-for-accessing-data/>
"example_shp"

#' Example population raster
#'
#' An example population raster from WorldPop for Kapiri Mposhi district in Zambia.
#'
#' @format ## `example_pop`
#' A RasterLayer object
#'
#' @source <https://github.com/wpgp/wopr/>
"example_pop"

#' Example location data
#'
#' Simulated locations meant to represent health facility locations in Kapiri Mposhi disttict, Zambia.
#'
#' @format ## `example_locs`
#' A data frame with 100 rows and 4 columns:
#' \describe{
#'   \item{label}{Unique identifier for each location}
#'   \item{x, y}{The longitude (x) and latitude (y) for each facility}
#'   \item{weight}{The initial weight used to estimate facility "attractiveness"}
#'   ...
#' }
#'
#' @source <>
"example_locs"
