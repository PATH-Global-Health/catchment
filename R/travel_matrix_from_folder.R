#' Title
#'
#' @param dir A character string for the location of individual travel time rasters (NOTE: this folder should ONLY contain .tif files for travel time rasters).
#' @param reference A raster files used to determine which pixels should be included (typically this is the population raster).
#' @param sparse TRUE/FALSE: Should pixels with NA values be remove? Almost always this should be TRUE.
#' @param progress TRUE/FALSE Show a progress bar?
#'
#' @return A matrix that is n_pixels by n_facilities.
#' @export
#'
#' @import progress
#'
#' @importFrom raster raster getValues
#' @importFrom fs dir_ls
#'
travel_mat_from_folder <- function(
  dir,
  reference = NA,
  sparse    = TRUE,
  progress  = TRUE) {

  # Get matrix params
  if(sparse) {
    valid_pix_index <- which(
      !is.na(raster::getValues(reference)) & raster::getValues(reference) > 0)
    n_pix <- length(valid_pix_index)
  } else {
    warning(
      "Retaining pixels with no population/predictive value will result in dense matrices and increased computational costs.")
    valid_pix_index <- numeric(1, length = length(raster::getValues(reference)))
    n_pix <- length(valid_pix_index)
  }

  raster_list <- fs::dir_ls(dir, glob = "*.tif")

  message("Matrix dimension: ", length(valid_pix_index), " pixels (rows) by ",
          length(raster_list), " locations (columns).")

  travel_matrix <- matrix(
    NA,
    nrow = length(valid_pix_index),
    ncol = length(raster_list))

  # Construct travel matrix using for loop
  if(progress){
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent :eta",
      total = length(raster_list), width = 80)}

  for(i in 1:length(raster_list)){
    tr <- raster::getValues(raster::raster(raster_list[i]))[valid_pix_index]
    travel_matrix[,i] <- tr
    if(progress){pb$tick()}
  }

  class(travel_matrix) <- c("travel_mat", class(travel_matrix))

  return(travel_matrix)
}
