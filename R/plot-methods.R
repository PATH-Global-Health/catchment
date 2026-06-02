#' Dominant-catchment raster
#'
#' Returns a [terra::SpatRaster] assigning each valid pixel to the facility it
#' is most likely to attend (the argmax over the reconstructed access
#' probabilities).  The raster is categorical, with levels labelled by facility
#' ID.
#'
#' @param mod A \code{catchment_fit} object from [catchment_model()].
#'
#' @return A categorical [terra::SpatRaster] named \code{dominant_facility}.
#' @export
#'
dominant_catchment <- function(mod) {

  if (!inherits(mod, "catchment_fit"))
    stop("`mod` must be a catchment_fit object from catchment_model().",
         call. = FALSE)

  prob_mat_new <- .reconstruct_probs(mod)          # [n_pixel x n_hf]
  dom          <- max.col(prob_mat_new, ties.method = "first")
  labels       <- mod$data$loc_labels

  out_raster <- mod$data$pop_raster
  pop_vals   <- terra::values(out_raster, mat = FALSE)
  valid      <- !is.na(pop_vals) & pop_vals > 0

  out_vals        <- rep(NA_integer_, length(pop_vals))
  out_vals[valid] <- dom
  terra::values(out_raster) <- out_vals

  levels(out_raster) <- data.frame(
    id    = seq_along(labels),
    label = labels,
    stringsAsFactors = FALSE
  )
  names(out_raster) <- "dominant_facility"
  out_raster
}


#' Plot a catchment_fit object
#'
#' Default plot for a fitted model: the dominant-catchment map (each pixel
#' coloured by the facility it is most likely to attend).  Uses \pkg{ggplot2}
#' and \pkg{tidyterra} when available, otherwise falls back to [terra::plot()].
#'
#' @param x A \code{catchment_fit} object.
#' @param ... Further arguments passed to [terra::plot()] in the fallback path.
#'
#' @return A \pkg{ggplot2} object (invisibly) when \pkg{ggplot2}/\pkg{tidyterra}
#'   are available; otherwise \code{NULL} invisibly after base plotting.
#' @method plot catchment_fit
#' @export
#'
plot.catchment_fit <- function(x, ...) {

  r <- dominant_catchment(x)

  have_gg <- requireNamespace("ggplot2", quietly = TRUE) &&
             requireNamespace("tidyterra", quietly = TRUE)

  if (!have_gg) {
    terra::plot(r, ...)
    return(invisible(NULL))
  }

  p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = r) +
    ggplot2::scale_fill_discrete(na.value = "transparent",
                                 name = "Facility") +
    ggplot2::labs(title = "Dominant catchment",
                  subtitle = "Most-likely facility per pixel") +
    ggplot2::theme_minimal()

  print(p)
  invisible(p)
}


#' Plot the access-probability surface for one facility
#'
#' Convenience wrapper around [get_prob_raster()] that renders the per-pixel
#' probability of attending a chosen facility.  Uses \pkg{ggplot2} and
#' \pkg{tidyterra} when available, otherwise falls back to [terra::plot()].
#'
#' @param mod A \code{catchment_fit} object from [catchment_model()].
#' @param id_label Character: the facility label whose surface to plot.
#'
#' @return A \pkg{ggplot2} object (invisibly) when \pkg{ggplot2}/\pkg{tidyterra}
#'   are available; otherwise \code{NULL} invisibly after base plotting.
#' @export
#'
plot_prob_surface <- function(mod, id_label) {

  if (!inherits(mod, "catchment_fit"))
    stop("`mod` must be a catchment_fit object from catchment_model().",
         call. = FALSE)

  r <- get_prob_raster(mod, id_label)

  have_gg <- requireNamespace("ggplot2", quietly = TRUE) &&
             requireNamespace("tidyterra", quietly = TRUE)

  if (!have_gg) {
    terra::plot(r, main = paste("Access probability:", id_label))
    return(invisible(NULL))
  }

  p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = r) +
    ggplot2::scale_fill_viridis_c(na.value = "transparent",
                                  name = "P(attend)") +
    ggplot2::labs(title = "Access-probability surface",
                  subtitle = paste("Facility:", id_label)) +
    ggplot2::theme_minimal()

  print(p)
  invisible(p)
}
