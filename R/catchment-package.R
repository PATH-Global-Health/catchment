#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#' @importFrom fs dir_ls
#' @importFrom fs path
#' @importFrom gdistance accCost
#' @importFrom gdistance geoCorrection
#' @importFrom gdistance transition
#' @importFrom Matrix Matrix
#' @importFrom parallel detectCores
#' @importFrom raster crop
#' @importFrom raster extent
#' @importFrom raster getValues
#' @importFrom raster projectRaster
#' @importFrom raster raster
#' @importFrom raster writeRaster
#' @importFrom Rcpp sourceCpp
#' @importFrom snow makeSOCKcluster
#' @importFrom snow stopCluster
#' @importFrom sp proj4string
#' @useDynLib catchment, .registration = TRUE
## usethis namespace: end
NULL
