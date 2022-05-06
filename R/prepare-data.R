#' Preparing input data from catchment model
#'
#' @param prob_mat_init A matrix object.
#' @param pop_raster A raster object/
#' @param location_data A dataframe object which contains point-level data, including coordinates, labels, and weights.
#' @param id_col A character for the column in location_data that contains the labels for individual point. This should be a unique identifier (no duplicated values).
#' @param weight_col A character for the column in location_data that contains the weights for each point.
#' @param x_col A character for the X column
#' @param y_col A character for the Y column
#' @param ...
#'
#' @return a list.
#' @export
#'
#'
prepare_data <- function(
  prob_mat_init,
  pop_raster,
  location_data,
  id_col = "label",
  weight_col = "weight",
  x_col = "x",
  y_col = "y",
  mesh.args = NULL) {

  # Extract components from location data
  # weights <- location_data[, which(names(location_data) %in% c(weight_col))]
  # loc_coords <- data.frame(
  #   x = location_data[, which(names(location_data) %in% c(x_col))],
  #   y = location_data[, which(names(location_data) %in% c(y_col))])
  # loc_labels <- location_data[, which(names(location_data) %in% c(id_col))]

  weights <- dplyr::pull(location_data, weight_col)
  loc_coords <- data.frame(
    x = dplyr::pull(location_data, x_col),
    y = dplyr::pull(location_data, y_col))
  loc_labels <- dplyr::pull(location_data, id_col)

  # Get pixel index
  valid_pix_index <- which(
    !is.na(raster::getValues(pop_raster)) &
      raster::getValues(pop_raster) > 0)

  # Get pixel locations
  pixel_locs <- raster::coordinates(pop_raster)[valid_pix_index,]

  # Population vector
  pop_vec <- raster::values(pop_raster)[valid_pix_index]

  # Make sure initial probability matrix is sparse
  if("travel_mat" %in% class(prob_mat_init)){
    initial_access_surface(prob_mat_init)
    }
  # if("access_mat" %in% class(prob_mat_init) & !"Matrix" %in% class(prob_mat_init)) {
  #   class(prob_mat_init) <- class(prob_mat_init)[!class(prob_mat)%in%"access_mat"]
  #   prob_mat_init <- Matrix::Matrix(t(prob_mat_init), sparse = T)}

  # Check dimensions


  # Create INLA mesh
  mesh <- build_mesh(pixel_locs, mesh.args = mesh.args)

  # Return
  out <- list(
    "pop_raster" = pop_raster,
    "prob_mat_init" = prob_mat_init,
    "weights" = weights,
    "which_not_NA" = as.numeric(!is.na(weights)),
    "pop_vec" = pop_vec,
    "pixel_coords" = pixel_locs,
    "loc_coords" = loc_coords,
    "loc_labels" = loc_labels,
    "mesh" = mesh
  )

  class(out) <- c("catchment_data", "list")

  return(out)


}

#' Create INLA mesh
#'
#' @param pixel_locs  A two-column matrix containing pixel coordinates
#' @param mesh.args  A list contains arugments from INLA::inla.mesh.2d()
#'
#' @return
#' @export
#'
#' @importFrom INLA inla.mesh.2d
#'
build_mesh <- function(pixel_locs, mesh.args = mesh.args, verbose = F, ...) {


  if(!is.null(mesh.args)) stopifnot(inherits(mesh.args, 'list'))

  pars <- list(cutoff = 0.2,
               max.edge = c(0.4, 2))
  pars[names(mesh.args)] <- mesh.args

  mesh <- INLA::inla.mesh.2d(
    loc = pixel_locs,
    max.edge = pars$max.edge,
    cutoff = pars$cutoff)

  if(verbose){print(pars)}


  return(mesh)
}


