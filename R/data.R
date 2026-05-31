#' Example boundary
#'
#' An example boundary of Kapiri Mposhi district in Zambia.
#'
#' @format ## `example_shp`
#' An `sf` object with a single polygon feature (CRS EPSG:4326).
#'
#' @source <https://malariaatlas.org/malariaatlas-r-package-for-accessing-data/>
#' @name example_shp
#' @docType data
#' @keywords datasets
NULL

#' Example location data
#'
#' Simulated locations meant to represent health facility locations in Kapiri
#' Mposhi district, Zambia.
#'
#' @format ## `example_locs`
#' A data frame with 100 rows and 4 columns:
#' \describe{
#'   \item{label}{Unique identifier for each location}
#'   \item{x, y}{The longitude (x) and latitude (y) for each facility}
#'   \item{weight}{The initial weight used to estimate facility "attractiveness"}
#' }
#'
#' @source Simulated for package examples.
#' @name example_locs
#' @docType data
#' @keywords datasets
NULL

#' Load the example population raster
#'
#' Loads an example WorldPop population raster for Kapiri Mposhi district,
#' Zambia, bundled with the package as a GeoTIFF. A function is used (rather than
#' a lazy-loaded dataset) because [terra::SpatRaster] objects cannot be
#' serialized into `.rda` files.
#'
#' @return A [terra::SpatRaster] (CRS EPSG:4326).
#' @export
#'
#' @importFrom terra rast
#'
#' @source <https://github.com/wpgp/wopr/>
#'
#' @examples
#' pop <- example_pop()
#' pop
example_pop <- function() {
  terra::rast(system.file("extdata", "example_pop.tif", package = "catchment"))
}
