#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom fs dir_ls
#' @importFrom fs path
#' @importFrom Matrix Matrix
#' @importFrom Rcpp sourceCpp
#' @importFrom stats runif
#' @importFrom terra rast costDist crop project ext vect cellFromXY writeRaster values crds same.crs crs
#' @useDynLib catchment, .registration = TRUE
## usethis namespace: end
NULL

# Quiet R CMD check about the .data pronoun used inside ggplot2 aes() mappings.
utils::globalVariables(".data")
