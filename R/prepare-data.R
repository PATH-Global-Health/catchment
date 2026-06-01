#' Preparing input data from catchment model
#'
#' @param prob_mat_init A matrix object.
#' @param pop_raster A raster object.
#' @param location_data A dataframe object which contains point-level data,
#'   including coordinates, labels, and weights.
#' @param id_col A character for the column in location_data that contains the
#'   labels for individual points.  Must be unique.
#' @param weight_col A character for the column in location_data that contains
#'   the weights (observed counts) for each facility.
#' @param x_col A character for the X column.
#' @param y_col A character for the Y column.
#' @param mesh.args A list of arguments passed to [build_mesh()].
#' @param pixel_covariates Optional pixel-level (demand-side) covariates.
#'   Either a multi-layer [terra::SpatRaster] whose values are extracted at the
#'   same valid pixels as \code{pop_raster}, or a numeric matrix/data frame
#'   already aligned to those pixels (one row per valid pixel).  Column names
#'   become covariate names.  Pass \code{NULL} (default) for no pixel
#'   covariates.
#' @param facility_covariates Optional facility-level (supply-side) covariates
#'   for the attraction mass term.  A numeric data frame or matrix with one row
#'   per facility, in the same order as \code{location_data}.  Pass \code{NULL}
#'   (default) for no facility covariates.
#' @param minimum_time,force_threshold,n_fac_limit Passed to
#'   [initial_access_surface()] when \code{prob_mat_init} is a raw
#'   \code{travel_mat}.  They also define the sparsity mask of the clamped travel
#'   template used by the C++ distance-decay path (Phase D); ignored when a
#'   pre-built probability matrix is supplied.
#'
#' @return a list with class `catchment_data`.
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
  mesh.args = NULL,
  pixel_covariates = NULL,
  facility_covariates = NULL,
  minimum_time = 10,
  force_threshold = 300,
  n_fac_limit = NULL) {

  # --- Input validation -------------------------------------------------------

  if (!inherits(pop_raster, "SpatRaster"))
    stop("`pop_raster` must be a SpatRaster.", call. = FALSE)

  if (!is.data.frame(location_data))
    stop("`location_data` must be a data frame.", call. = FALSE)

  for (col in c(id_col, weight_col, x_col, y_col)) {
    if (!col %in% names(location_data))
      stop("Column '", col, "' not found in `location_data`.", call. = FALSE)
  }

  weights <- dplyr::pull(location_data, weight_col)
  loc_coords <- data.frame(
    x = dplyr::pull(location_data, x_col),
    y = dplyr::pull(location_data, y_col))
  loc_labels <- dplyr::pull(location_data, id_col)

  n_fac <- nrow(location_data)

  if (anyDuplicated(loc_labels))
    stop("`id_col` values must be unique; duplicates found in '", id_col, "'.",
         call. = FALSE)

  if (!is.numeric(weights) || length(weights) != n_fac)
    stop("`weight_col` must be a numeric column with one value per facility.",
         call. = FALSE)

  # Check that prob_mat_init columns/rows agree with n_fac.
  # initial_access_surface(sparse=TRUE) → [n_hf x n_pixel] sparse Matrix;
  # sparse=FALSE → [n_pixel x n_hf] dense matrix.
  if (!inherits(prob_mat_init, "travel_mat")) {
    if (inherits(prob_mat_init, "Matrix")) {
      if (nrow(prob_mat_init) != n_fac)
        stop("`prob_mat_init` has ", nrow(prob_mat_init),
             " rows but `location_data` has ", n_fac,
             " facilities (expected one row per facility for a sparse Matrix).",
             call. = FALSE)
    } else if (is.matrix(prob_mat_init)) {
      if (ncol(prob_mat_init) != n_fac)
        stop("`prob_mat_init` has ", ncol(prob_mat_init),
             " columns but `location_data` has ", n_fac,
             " facilities (expected one column per facility for a dense matrix).",
             call. = FALSE)
    }
  }

  # Get pixel index
  pop_vals <- terra::values(pop_raster, mat = FALSE)
  valid_pix_index <- which(!is.na(pop_vals) & pop_vals > 0)

  # Get pixel locations
  pixel_locs <- terra::crds(pop_raster, na.rm = FALSE)[valid_pix_index, ]

  # Population vector
  pop_vec <- pop_vals[valid_pix_index]

  # When a raw travel matrix is supplied, also build a clamped sparse travel
  # template for the C++ decay path (Phase D).  Its sparsity pattern is taken
  # directly from initial_access_surface() so it matches the legacy mask exactly.
  travel_sparse <- NULL
  if(inherits(prob_mat_init, "travel_mat")){
    travel_sparse <- .build_travel_sparse(prob_mat_init,
                                          minimum_time = minimum_time,
                                          force_threshold = force_threshold,
                                          n_fac_limit = n_fac_limit)
    prob_mat_init <- initial_access_surface(prob_mat_init,
                                            minimum_time = minimum_time,
                                            force_threshold = force_threshold,
                                            n_fac_limit = n_fac_limit,
                                            sparse = FALSE)
    }
  # if("access_mat" %in% class(prob_mat_init) & !"Matrix" %in% class(prob_mat_init)) {
  #   class(prob_mat_init) <- class(prob_mat_init)[!class(prob_mat)%in%"access_mat"]
  #   prob_mat_init <- Matrix::Matrix(t(prob_mat_init), sparse = T)}

  # Check dimensions


  # Create INLA mesh
  mesh <- build_mesh(pixel_locs, mesh.args = mesh.args)

  # --- Pixel-level covariates -------------------------------------------------
  n_valid_pixel <- length(valid_pix_index)

  X_pixel <- .process_pixel_covariates(pixel_covariates, pop_raster,
                                        valid_pix_index, n_valid_pixel)

  # --- Facility-level covariates ----------------------------------------------
  Z_hf <- .process_facility_covariates(facility_covariates, n_fac)

  # Return
  out <- list(
    "pop_raster"  = pop_raster,
    "prob_mat_init" = prob_mat_init,
    "weights"     = weights,
    "which_not_NA" = as.numeric(!is.na(weights)),
    "pop_vec"     = pop_vec,
    "pixel_coords" = pixel_locs,
    "loc_coords"  = loc_coords,
    "loc_labels"  = loc_labels,
    "mesh"        = mesh,
    "X_pixel"     = X_pixel,
    "Z_hf"        = Z_hf,
    "travel_sparse" = travel_sparse
  )

  class(out) <- c("catchment_data", "list")

  return(out)


}

#' Print a catchment_data object
#' @param x A \code{catchment_data} object.
#' @param ... Further arguments (currently unused).
#' @return Invisibly returns \code{x}.
#' @method print catchment_data
#' @export
print.catchment_data <- function(x, ...) {
  cat("catchment_data\n")
  cat(" Facilities  :", length(x$loc_labels), "\n")
  cat(" Pop pixels  :", length(x$pop_vec), "\n")
  cat(" Mesh nodes  :", nrow(x$mesh$loc), "\n")
  cat(" Weight range:", round(min(x$weights, na.rm = TRUE), 1), "-",
      round(max(x$weights, na.rm = TRUE), 1), "\n")
  if (!is.null(x$X_pixel) && ncol(x$X_pixel) > 0)
    cat(" Pixel covs  :", ncol(x$X_pixel), "(", paste(colnames(x$X_pixel), collapse = ", "), ")\n")
  if (!is.null(x$Z_hf) && ncol(x$Z_hf) > 0)
    cat(" Fac covs    :", ncol(x$Z_hf),   "(", paste(colnames(x$Z_hf),    collapse = ", "), ")\n")
  invisible(x)
}

#' Summarise a catchment_data object
#' @param object A \code{catchment_data} object.
#' @param ... Further arguments (currently unused).
#' @return Invisibly returns \code{object}.
#' @method summary catchment_data
#' @export
summary.catchment_data <- function(object, ...) {
  cat("catchment_data summary\n")
  cat(" Facilities       :", length(object$loc_labels), "\n")
  cat(" Pop pixels (>0)  :", length(object$pop_vec), "\n")
  cat(" Total population :", round(sum(object$pop_vec)), "\n")
  cat(" Mesh nodes       :", nrow(object$mesh$loc), "\n")
  cat(" Facilities (NA)  :", sum(is.na(object$weights)), "\n")
  cat(" Weight summary   :\n")
  print(summary(object$weights))
  invisible(object)
}

# Internal: build a clamped sparse travel-time template [n_hf × n_pixel] for the
# C++ distance-decay path. Stored values are clamped travel times (minutes); the
# sparsity pattern is taken from initial_access_surface()'s nonzero pattern so it
# is identical to the legacy mask. Because the decay is monotone in travel time,
# the kept set is decay-shape invariant, and a power decay with exponent 2
# reproduces the legacy 1/d^2 surface exactly.
.build_travel_sparse <- function(travel_matrix, minimum_time = 10,
                                 force_threshold = 300, n_fac_limit = NULL) {
  tt <- unclass(travel_matrix)            # [n_pixel × n_hf]
  if (anyNA(tt)) tt[is.na(tt)] <- max(tt, na.rm = TRUE)
  if (!is.null(minimum_time)) tt[tt <= minimum_time] <- minimum_time

  leg <- suppressMessages(
    initial_access_surface(travel_matrix, minimum_time = minimum_time,
                           force_threshold = force_threshold,
                           n_fac_limit = n_fac_limit, normalized = FALSE,
                           sparse = FALSE)
  )
  tt[leg == 0] <- 0
  Matrix::Matrix(t(tt), sparse = TRUE)    # [n_hf × n_pixel]
}

# Internal: validate and extract pixel-level covariates.
# Returns a numeric matrix [n_valid_pixel × p] with p >= 0.
.process_pixel_covariates <- function(pixel_covariates, pop_raster,
                                      valid_pix_index, n_valid_pixel) {
  if (is.null(pixel_covariates))
    return(matrix(0, nrow = n_valid_pixel, ncol = 0))

  if (inherits(pixel_covariates, "SpatRaster")) {
    # Extract all layers at valid pixel positions (row-major, no NA-removal)
    all_vals <- terra::values(pixel_covariates, mat = TRUE)
    X <- all_vals[valid_pix_index, , drop = FALSE]
    if (is.null(colnames(X)))
      colnames(X) <- paste0("pixel_cov", seq_len(ncol(X)))
    if (any(!is.finite(X)))
      stop("pixel_covariates contains non-finite values at valid population pixels.",
           call. = FALSE)
    return(X)
  }

  # Matrix or data frame
  if (is.data.frame(pixel_covariates)) pixel_covariates <- as.matrix(pixel_covariates)
  if (!is.matrix(pixel_covariates))
    stop("`pixel_covariates` must be a SpatRaster, matrix, or data frame.",
         call. = FALSE)
  if (nrow(pixel_covariates) != n_valid_pixel)
    stop("`pixel_covariates` has ", nrow(pixel_covariates),
         " rows but there are ", n_valid_pixel, " valid pixels.", call. = FALSE)
  if (!is.numeric(pixel_covariates))
    stop("`pixel_covariates` must be numeric.", call. = FALSE)
  if (is.null(colnames(pixel_covariates)))
    colnames(pixel_covariates) <- paste0("pixel_cov", seq_len(ncol(pixel_covariates)))
  return(pixel_covariates)
}

# Internal: validate facility-level covariates.
# Returns a numeric matrix [n_hf × p] with p >= 0.
.process_facility_covariates <- function(facility_covariates, n_fac) {
  if (is.null(facility_covariates))
    return(matrix(0, nrow = n_fac, ncol = 0))

  if (is.data.frame(facility_covariates))
    facility_covariates <- as.matrix(facility_covariates)
  if (!is.matrix(facility_covariates))
    stop("`facility_covariates` must be a data frame or matrix.", call. = FALSE)
  if (nrow(facility_covariates) != n_fac)
    stop("`facility_covariates` has ", nrow(facility_covariates),
         " rows but `location_data` has ", n_fac, " facilities.", call. = FALSE)
  if (!is.numeric(facility_covariates))
    stop("`facility_covariates` must be numeric.", call. = FALSE)
  if (is.null(colnames(facility_covariates)))
    colnames(facility_covariates) <- paste0("fac_cov", seq_len(ncol(facility_covariates)))
  return(facility_covariates)
}

#' Create INLA mesh
#'
#' @param pixel_locs A two-column matrix containing pixel coordinates.
#' @param mesh.args A list of arguments passed to [fmesher::fm_mesh_2d_inla()].
#' @param verbose TRUE/FALSE: print the mesh arguments used.
#' @param ... Additional arguments (currently unused).
#'
#' @return An `fm_mesh_2d` object (also of class `inla.mesh`).
#' @export
#'
build_mesh <- function(pixel_locs, mesh.args = mesh.args, verbose = F, ...) {

  if (!requireNamespace("fmesher", quietly = TRUE)) {
    stop("Package 'fmesher' is required for build_mesh(). Install it from CRAN ",
         "with install.packages(\"fmesher\").", call. = FALSE)
  }

  if(!is.null(mesh.args)) stopifnot(inherits(mesh.args, 'list'))

  pars <- list(cutoff = 0.2,
               max.edge = c(0.4, 2))
  pars[names(mesh.args)] <- mesh.args

  mesh <- fmesher::fm_mesh_2d_inla(
    loc = pixel_locs,
    max.edge = pars$max.edge,
    cutoff = pars$cutoff)

  if(verbose){print(pars)}


  return(mesh)
}


