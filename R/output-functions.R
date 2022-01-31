

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

  for(i in 1:length(mod$data$weights)){
    prob_mat_new[,i] <- prob_mat_new[,i] * updated_wgts[i]
  }

  for(i in 1:nrow(prob_mat_new)){
    prob_mat_new[i,] <- prob_mat_new[i,]/sum(prob_mat_new[i,])
  }

  out <- as.vector(t(prob_mat_new) %*% mod$data$pop_vec)
  names(out) <- mod$data$loc_labels

  return(out)

}
