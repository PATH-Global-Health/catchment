# Isolate and plot attendance probability surfaces

#' Get access probability raster from catchment mode
#'
#' @param mod Catchment model object
#' @param id_label A character string containing the label ID for the specified location
#'
#' @importFrom raster values
#'
#' @return A RasterLayer object
#' @export
#'
get_prob_raster <- function(mod, id_label) {

  # Get update probability matrix
  op <- mod$obj$env$last.par.best
  wgts <- unname(exp(op[names(op) == "log_hf_mass"]))
  prob_mat_new <- mod$data$prob_mat_init

  # Re-weight
  for(i in 1:length(wgts)){
    prob_mat_new[,i] <- prob_mat_new[,i] * wgts[i]
  }

  # Normalize
  for(i in 1:nrow(prob_mat_new)){
    prob_mat_new[i,] <- prob_mat_new[i,]/sum(prob_mat_new[i,])
  }

  # Check normalization
  # summary(rowSums(prob_mat_new))

  # Get selected probabilty surface
  id <- which(mod$data$loc_labels == id_label)
  prob_surface <- mod$data$pop_raster
  raster::values(prob_surface)[!is.na(raster::values(prob_surface))] <- prob_mat_new[,id]
  names(prob_surface) = "access_probability"
  # plot(prob_surface)
  return(prob_surface)
}
