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
#' Simulated health facility locations for Kapiri Mposhi district, Zambia. The
#' 30 facilities are placed at population-weighted random pixels, and the
#' \code{weight} column holds simulated observed event counts: population is
#' allocated to facilities through a known exponential distance-decay
#' (\code{tau = 45} minutes, using a walk-only friction surface) and mild
#' facility masses, then thinned to counts with Poisson noise. These counts give
#' a well-identified model that converges with a positive-definite Hessian, so
#' the uncertainty and validation tooling can be demonstrated. See
#' \code{inst/generate-example-data.R}.
#'
#' @format ## `example_locs`
#' A data frame with 30 rows and 4 columns:
#' \describe{
#'   \item{label}{Unique identifier for each facility}
#'   \item{x, y}{The longitude (x) and latitude (y) for each facility}
#'   \item{weight}{The observed event count for each facility (model response)}
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
