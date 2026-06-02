#' Draw posterior-predictive facility counts
#'
#' Simulates replicate facility counts from a fitted catchment model, using the
#' fitted expected counts (\code{case_hf}) and the model's likelihood family.
#' For the negative-binomial family the TMB parameterisation is mirrored exactly
#' (\code{var = mu + mu^2 / size}, with \code{var - mu = exp(log_nb_phi)} held
#' constant, so \code{size = mu^2 / exp(log_nb_phi)}).
#'
#' @param mod A \code{catchment_fit} object from [catchment_model()].
#' @param nsim Number of replicate datasets to draw (default 1000).
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A numeric matrix \code{[nsim x n_facilities]} of simulated counts,
#'   with columns named by facility label.
#' @export
posterior_predict <- function(mod, nsim = 1000, seed = NULL) {

  if (!inherits(mod, "catchment_fit"))
    stop("`mod` must be a catchment_fit object from catchment_model().",
         call. = FALSE)
  if (!is.null(seed)) set.seed(seed)

  mu     <- mod$obj$report()$case_hf
  n_hf   <- length(mu)
  family <- if (!is.null(mod$family)) mod$family else "poisson"

  if (family == "poisson") {
    draws <- stats::rpois(nsim * n_hf, lambda = rep(mu, each = nsim))
  } else {
    phi  <- exp(unname(mod$fit$par["log_nb_phi"]))   # var - mu (constant)
    size <- mu^2 / phi
    draws <- stats::rnbinom(nsim * n_hf,
                            size = rep(size, each = nsim),
                            mu   = rep(mu,   each = nsim))
  }

  out <- matrix(draws, nrow = nsim, ncol = n_hf)
  colnames(out) <- mod$data$loc_labels
  out
}


#' Posterior-predictive check for a fitted catchment model
#'
#' Compares observed facility counts with their posterior-predictive
#' distribution, reporting per-facility predictive intervals, interval
#' coverage, and a dispersion statistic (the ratio of the realised
#' predictive variance to its mean, averaged over facilities; ~1 indicates
#' Poisson-consistent dispersion).
#'
#' @param mod A \code{catchment_fit} object from [catchment_model()].
#' @param nsim Number of posterior-predictive replicates (default 1000).
#' @param prob Width of the central predictive interval (default 0.95).
#' @param seed Optional integer seed.
#' @param plot Logical (default \code{TRUE}); attach a \pkg{ggplot2} plot of
#'   observed counts against predictive intervals (requires \pkg{ggplot2}).
#'
#' @return Invisibly, a list with \code{summary} (a data frame of observed,
#'   predicted mean, and interval bounds per facility), \code{coverage} (the
#'   fraction of observed counts inside their predictive interval),
#'   \code{dispersion}, and (if requested) \code{plot}.
#' @export
pp_check <- function(mod, nsim = 1000, prob = 0.95, seed = NULL, plot = TRUE) {

  if (!inherits(mod, "catchment_fit"))
    stop("`mod` must be a catchment_fit object from catchment_model().",
         call. = FALSE)

  yrep <- posterior_predict(mod, nsim = nsim, seed = seed)
  obs  <- mod$data$weights
  keep <- mod$data$which_not_NA == 1            # observed facilities only

  a   <- (1 - prob) / 2
  qs  <- apply(yrep, 2, stats::quantile, probs = c(a, 0.5, 1 - a), na.rm = TRUE)
  mu  <- colMeans(yrep)

  summary_df <- data.frame(
    label    = mod$data$loc_labels,
    observed = obs,
    pred_mean = mu,
    lower    = qs[1, ],
    median   = qs[2, ],
    upper    = qs[3, ],
    observed_flag = keep,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  inside   <- with(summary_df, observed >= lower & observed <= upper)
  coverage <- mean(inside[keep])

  # Dispersion: mean over facilities of Var(yrep)/Mean(yrep).
  vrep <- apply(yrep, 2, stats::var)
  dispersion <- mean(vrep[keep] / pmax(mu[keep], .Machine$double.eps))

  out <- list(summary = summary_df, coverage = coverage,
              dispersion = dispersion)

  if (plot) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      warning("Package 'ggplot2' is required for the pp_check plot; ",
              "returning numeric output only.", call. = FALSE)
    } else {
      df <- summary_df[keep, , drop = FALSE]
      df$label <- factor(df$label, levels = df$label[order(df$observed)])
      out$plot <- ggplot2::ggplot(df, ggplot2::aes(x = .data$label)) +
        ggplot2::geom_linerange(ggplot2::aes(ymin = .data$lower,
                                             ymax = .data$upper),
                                colour = "grey60") +
        ggplot2::geom_point(ggplot2::aes(y = .data$pred_mean),
                            colour = "steelblue") +
        ggplot2::geom_point(ggplot2::aes(y = .data$observed),
                            colour = "firebrick", shape = 4, size = 2) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = "Facility", y = "Count",
                      title = "Posterior-predictive check",
                      subtitle = "x = observed; point = predicted mean; line = interval")
    }
  }

  invisible(out)
}


#' Leave-one-facility-out cross-validation
#'
#' Refits the model with each observed facility's count held out in turn (its
#' likelihood contribution is dropped via \code{which_not_NA}), then predicts
#' the held-out expected count.  The refits reuse the original family and decay
#' configuration but skip [TMB::sdreport()] for speed.
#'
#' @param mod A \code{catchment_fit} object from [catchment_model()].
#'
#' @return A data frame with columns \code{label}, \code{observed},
#'   \code{predicted}, and \code{error} (predicted - observed).  The overall
#'   \code{rmse} and \code{mae} are attached as attributes.
#' @export
loo_facility_cv <- function(mod) {

  if (!inherits(mod, "catchment_fit"))
    stop("`mod` must be a catchment_fit object from catchment_model().",
         call. = FALSE)

  dat            <- mod$data
  family         <- mod$family
  decay          <- if (!is.null(mod$decay)) mod$decay else "none"
  estimate_decay <- "log_decay" %in% names(mod$fit$par)
  decay_init     <- if (decay == "none") NULL else mod$decay_param

  obs_idx <- which(dat$which_not_NA == 1)
  preds   <- rep(NA_real_, length(obs_idx))

  for (k in seq_along(obs_idx)) {
    i <- obs_idx[k]
    dat_k <- dat
    dat_k$which_not_NA[i] <- 0

    obj <- suppressMessages(make_model_object(
      dat_k, family = family, decay = decay,
      estimate_decay = estimate_decay, decay_init = decay_init))
    fit <- stats::nlminb(obj$par, obj$fn, obj$gr,
                         control = list(iter.max = 300, eval.max = 300))
    preds[k] <- obj$report()$case_hf[i]
  }

  observed <- dat$weights[obs_idx]
  err      <- preds - observed

  out <- data.frame(
    label     = dat$loc_labels[obs_idx],
    observed  = observed,
    predicted = preds,
    error     = err,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  attr(out, "rmse") <- sqrt(mean(err^2))
  attr(out, "mae")  <- mean(abs(err))
  out
}
