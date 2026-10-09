#' Build a travel-time matrix from a folder of rasters
#'
#' Reads every `.tif` in `dir` (one travel-time surface per facility, as written
#' by [create_travel_surface()] with `individual_surfaces = TRUE`) into a
#' pixel-by-facility matrix.
#'
#' Columns are in file-name order, which is generally **not** the row order of
#' your facility table.  Each column is therefore named after its file (without
#' the `.tif` extension), and [prepare_data()] uses those names to match columns
#' to `location_data[[id_col]]`.  Keep the column names intact, and name the
#' rasters by facility id.
#'
#' @param dir A character string for the location of individual travel time rasters (NOTE: this folder should ONLY contain .tif files for travel time rasters, each named `<facility id>.tif`).
#' @param reference A raster files used to determine which pixels should be included (typically this is the population raster).
#' @param sparse TRUE/FALSE: Should pixels with NA values be remove? Almost always this should be TRUE.
#' @param progress TRUE/FALSE Show a progress bar?
#'
#' @return A matrix of class `travel_mat` that is n_pixels by n_facilities, with
#'   column names taken from the `.tif` file names (extension removed).
#' @export
#'
#' @import progress
#'
#' @importFrom terra rast values
#' @importFrom fs dir_ls path_file path_ext_remove
#'
travel_mat_from_folder <- function(
  dir,
  reference = NA,
  sparse    = TRUE,
  progress  = TRUE) {

  ref_vals <- terra::values(reference, mat = FALSE)

  # Get matrix params
  if(sparse) {
    valid_pix_index <- which(!is.na(ref_vals) & ref_vals > 0)
    n_pix <- length(valid_pix_index)
  } else {
    warning(
      "Retaining pixels with no population/predictive value will result in dense matrices and increased computational costs.")
    valid_pix_index <- seq_along(ref_vals)
    n_pix <- length(valid_pix_index)
  }

  raster_list <- fs::dir_ls(dir, glob = "*.tif")

  message("Matrix dimension: ", length(valid_pix_index), " pixels (rows) by ",
          length(raster_list), " locations (columns).")

  travel_matrix <- matrix(
    NA,
    nrow = length(valid_pix_index),
    ncol = length(raster_list),
    dimnames = list(NULL, as.character(fs::path_ext_remove(fs::path_file(raster_list)))))

  # Construct travel matrix using for loop
  if(progress){
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent :eta",
      total = length(raster_list), width = 80)}

  for(i in 1:length(raster_list)){
    tr <- terra::values(terra::rast(raster_list[i]), mat = FALSE)[valid_pix_index]
    travel_matrix[,i] <- tr
    if(progress){pb$tick()}
  }

  class(travel_matrix) <- c("travel_mat", class(travel_matrix))

  return(travel_matrix)
}
