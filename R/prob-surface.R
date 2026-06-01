# Isolate and plot attendance probability surfaces

#' Get access probability raster from catchment mode
#'
#' @param mod Catchment model object
#' @param id_label A character string containing the label ID for the specified location
#'
#' @importFrom terra values
#'
#' @return A [terra::SpatRaster] object
#' @export
#'
get_prob_raster <- function(mod, id_label) {

  # Get update probability matrix
  op <- mod$obj$env$last.par.best
  wgts <- unname(exp(op[names(op) == "log_hf_mass"]))
  prob_mat_new <- mod$data$prob_mat_init

  # Re-weight each facility (column) by its mass, then row-normalize
  prob_mat_new <- prob_mat_new * rep(wgts, each = nrow(prob_mat_new))
  prob_mat_new <- prob_mat_new / rowSums(prob_mat_new)

  # Get selected probabilty surface
  id <- which(mod$data$loc_labels == id_label)
  prob_surface <- mod$data$pop_raster

  # Match the valid-pixel mask used in prepare_data() (non-NA and > 0)
  pop_vals <- terra::values(prob_surface, mat = FALSE)
  valid <- !is.na(pop_vals) & pop_vals > 0

  out_vals <- rep(NA_real_, length(pop_vals))
  out_vals[valid] <- prob_mat_new[, id]
  terra::values(prob_surface) <- out_vals

  names(prob_surface) <- "access_probability"
  return(prob_surface)
}
