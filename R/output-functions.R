

# write_access_surfaces <- function() {}


#' Get estimated catchment populations
#'
#' @param mod Model object from catchment_model
#'
#' @return a named vector containing estimated catchment populations
#' @export
#'
catchment_populations <- function(mod){

  obj_params <- mod$obj$env$last.par.best
  updated_wgts <- unname(exp(obj_params[names(obj_params) == "log_hf_mass"]))

  prob_mat_new <- mod$data$prob_mat_init

  # Re-weight each facility (column) by its updated mass, then row-normalize
  prob_mat_new <- prob_mat_new * rep(updated_wgts, each = nrow(prob_mat_new))
  prob_mat_new <- prob_mat_new / rowSums(prob_mat_new)

  out <- as.vector(t(prob_mat_new) %*% mod$data$pop_vec)
  names(out) <- mod$data$loc_labels

  return(out)

}
