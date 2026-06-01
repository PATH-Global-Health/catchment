
#' Get estimated catchment populations
#'
#' Returns the estimated population served by each facility.  When
#' \code{uncertainty = FALSE} (default) a named numeric vector of point
#' estimates is returned, preserving the original behaviour.  When
#' \code{uncertainty = TRUE} a data frame is returned with columns
#' \code{label}, \code{estimate}, \code{se}, \code{lower}, and \code{upper}
#' (95 \% approximate CI via delta method from [TMB::sdreport()]).
#'
#' @param mod A \code{catchment_fit} object from [catchment_model()].
#' @param uncertainty Logical (default \code{FALSE}).  If \code{TRUE}, return
#'   a data frame with standard errors and 95 \% confidence intervals
#'   propagated through the random effects via [TMB::sdreport()].
#'
#' @return A named numeric vector (when \code{uncertainty = FALSE}) or a data
#'   frame (when \code{uncertainty = TRUE}).
#' @export
#'
catchment_populations <- function(mod, uncertainty = FALSE) {

  if (uncertainty) {
    if (is.null(mod$sdr))
      stop("sdreport is not available in this model object. ",
           "Re-run catchment_model() (sdreport is computed automatically).",
           call. = FALSE)

    sdr_rep  <- summary(mod$sdr, "report")
    pop_rows <- rownames(sdr_rep) == "pop_hf"

    if (!any(pop_rows))
      stop("pop_hf not found in sdreport output. ",
           "Ensure the model was compiled with ADREPORT(pop_hf).", call. = FALSE)

    est <- sdr_rep[pop_rows, "Estimate"]
    se  <- sdr_rep[pop_rows, "Std. Error"]

    out <- data.frame(
      label    = mod$data$loc_labels,
      estimate = est,
      se       = se,
      lower    = est - 1.96 * se,
      upper    = est + 1.96 * se,
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    return(out)
  }

  # --- Point estimate (original behaviour) ------------------------------------
  prob_mat_new <- .reconstruct_probs(mod)

  out <- as.vector(t(prob_mat_new) %*% mod$data$pop_vec)
  names(out) <- mod$data$loc_labels

  return(out)
}


# Internal: reconstruct the normalised [n_pixel x n_hf] probability matrix from a
# fitted model, applying the *learned* decay (Phase D) so the point-estimate and
# probability-raster outputs stay consistent with what the C++ template computed.
# For decay == "none" this reproduces the legacy fixed-surface behaviour.
.reconstruct_probs <- function(mod) {
  op    <- mod$obj$env$last.par.best
  mass  <- unname(exp(op[names(op) == "log_hf_mass"]))     # one per facility
  decay <- if (!is.null(mod$decay)) mod$decay else "none"

  if (decay == "none") {
    pm <- as.matrix(mod$data$prob_mat_init)                # [n_pixel x n_hf]
  } else {
    dpar <- mod$decay_param                                # resolved by catchment_model()
    pm   <- t(as.matrix(mod$data$travel_sparse))           # [n_pixel x n_hf]
    nz   <- pm != 0
    if (decay == "power") {
      pm[nz] <- pm[nz] ^ (-dpar)
    } else {
      pm[nz] <- exp(-pm[nz] / dpar)
    }
  }

  # Re-weight each facility (column) by its mass, then row-normalise (per pixel)
  pm <- pm * rep(mass, each = nrow(pm))
  pm <- pm / rowSums(pm)
  pm
}


#' Get access probability raster from catchment model
#'
#' Returns a [terra::SpatRaster] showing each pixel's probability of attending
#' the specified facility.
#'
#' @param mod A \code{catchment_fit} object from [catchment_model()].
#' @param id_label A character string: the label ID for the selected facility.
#' @param uncertainty Logical (default \code{FALSE}).  Pixel-level probability
#'   standard errors are not yet implemented; setting \code{TRUE} raises an
#'   informative error.
#'
#' @importFrom terra values
#'
#' @return A [terra::SpatRaster] object.
#' @export
#'
get_prob_raster <- function(mod, id_label, uncertainty = FALSE) {

  if (uncertainty)
    stop("Pixel-level probability uncertainty is not yet implemented. ",
         "Use uncertainty = FALSE (the default).", call. = FALSE)

  # Get updated probability matrix (applies the learned decay when active)
  prob_mat_new <- .reconstruct_probs(mod)

  # Get selected probability surface
  id           <- which(mod$data$loc_labels == id_label)
  prob_surface <- mod$data$pop_raster

  # Match the valid-pixel mask used in prepare_data() (non-NA and > 0)
  pop_vals <- terra::values(prob_surface, mat = FALSE)
  valid    <- !is.na(pop_vals) & pop_vals > 0

  out_vals         <- rep(NA_real_, length(pop_vals))
  out_vals[valid]  <- prob_mat_new[, id]
  terra::values(prob_surface) <- out_vals

  names(prob_surface) <- "access_probability"
  return(prob_surface)
}
