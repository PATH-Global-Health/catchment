#' Title
#'
#' @param travel_matrix A matrix containing travel times, which pixels indexed by rows and locations indexed by columns. Typically created with catchment::create_travel_matrix_from_folder().
#' @param transform A character, numeric, or function. If character, a predefined transformation will be applied. Currently available options are "inverse_dist_squared". If numeric, then an inverse function with a defined value for the exponential term (e.g., 1/x^value). If a function, then the user-supplied transformation is applied.
#' @param minimum_time A numeric used to defined a minimum travel time. Travel times below this value will be set to the minimum time, this helps prevent the initial probabilities from being overly skewed by small numbers.
#' @param force_threshold A numeric used to define the maximum travel distance value, beyond which pixels will be forced to their nearest facility.
#' @param normalized TRUE/FALSE Fix probabilities such that all rows (i.e., pixels) sum to 1.
#' @param sparse TRUE/FALSE return a sparse matrix used by catchment_model
#'
#' @return A matrix. If sparse == FALSE, then N_pixel rows by N_locations matrix containing initial assess surface will be returned. If sparse == TRUE, then a "sparse" Matrix object, used by model_catchment, is returned.
#' @export
#'
#' @importFrom Matrix Matrix
#'
#'
initial_access_surface <- function(
  travel_matrix,
  transform = "inverse_dist_squared",
  minimum_time = 10,
  force_threshold = 300,
  normalized = TRUE,
  sparse = TRUE) {

  # Make initial adjustments
  prob_mat <- travel_matrix

  if(sum(is.na(prob_mat)) > 0){
    message("Setting NA values from travel matrix to maximum value.")
    prob_mat[is.na(prob_mat)] <- max(prob_mat, na.rm = T)}

  if(!is.null(minimum_time)){
    message("Adjusting times less than ", minimum_time,
            " to improve normalization to proper probability.")
    prob_mat[prob_mat<=10] = 10
  } else {warning("Suggest setting minimum_time, otherwise probability  normalization may be overly skewed.")}

  # Apply transformation

  if(transform == "inverse_dist_squared") {
    message("Applying inverse distance squared transformation.")
    prob_mat <- 1/prob_mat^2
  } else if(class(transform) == "numeric") {
    message("Applying inverse distance transformation with user-defined exponential term.")
    prob_mat <- 1/prob_mat^transform
  } else {
    message("Applying user-defined transformation.")
    prob_mat <- transform(prob_mat)
  }

  # Apply force threshold
  if(!is.na(force_threshold)) {
    message("Forcing pixels over ",
            force_threshold, " minutes to travel to nearest facility.")
    which_zero <- which(apply(travel_matrix, 1, min) > force_threshold)
    prob_mat1 <- prob_mat
    prob_mat[travel_matrix > force_threshold] <- 0

    for(i in which_zero){
      which_max <- which.max(prob_mat1[i, ])
      prob_mat[i, which_max] <- 1
    }
  }
  # Normalize
  if(normalized){
    for(i in 1:nrow(prob_mat)){prob_mat[i,] <- prob_mat[i,]/sum(prob_mat[i,])}
  }

  class(prob_mat) <- class(prob_mat)[!class(prob_mat)%in%"travel_mat"]

  # Make sparse
  if(sparse) {
    prob_mat <- Matrix::Matrix(t(prob_mat), sparse = T)
    return(prob_mat)
    } else {
      # class(prob_mat) <- c("access_mat", class(prob_mat))
      return(prob_mat)
    }
}
