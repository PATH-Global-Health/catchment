

#' Fit catchment model
#'
#' Fits the gravity-style catchment model and computes parameter uncertainty
#' via [TMB::sdreport()].  Convergence is checked automatically and warnings are
#' issued when the optimiser reports failure, the max gradient is large, or the
#' Hessian is not positive-definite.
#'
#' @param dat A [catchment_data] object created by [prepare_data()].
#' @param family Likelihood family: \code{"poisson"} (default) or \code{"nb"}
#'   for negative binomial (opt-in overdispersion robustness).
#' @param decay Distance-decay family applied to travel time inside the model:
#'   \code{"exponential"} (default, \code{exp(-d/tau)}), \code{"power"}
#'   (\code{d^-a}), or \code{"none"} (use the precomputed decay baked into
#'   \code{prob_mat_init}).  \code{"exponential"} and \code{"power"} require a
#'   travel template (supply a raw travel matrix to [prepare_data()]); if absent
#'   the fit falls back to \code{"none"} with a message.
#' @param estimate_decay TRUE/FALSE: estimate the decay parameter (default
#'   \code{TRUE}).  When \code{FALSE} it is fixed at \code{decay_init}.  Ignored
#'   when \code{decay = "none"}.
#' @param decay_init Initial (natural-scale) decay parameter: the exponent
#'   \code{a} for \code{"power"} or the scale \code{tau} (minutes) for
#'   \code{"exponential"}.  Defaults to 2 (power) or 60 (exponential).
#' @param time TRUE/FALSE Print optimisation run time?
#'
#' @return A list of class \code{catchment_fit} containing:
#'   \describe{
#'     \item{obj}{The TMB AD object.}
#'     \item{fit}{The \code{nlminb} optimisation result.}
#'     \item{sdr}{The [TMB::sdreport()] result, or \code{NULL} if it failed.}
#'     \item{data}{The \code{catchment_data} list passed in.}
#'   }
#' @export
#'
catchment_model <- function(dat, family = "poisson",
                            decay = "exponential", estimate_decay = TRUE,
                            decay_init = NULL, time = TRUE) {

  if (!inherits(dat, "catchment_data"))
    stop("`dat` must be a catchment_data object created by prepare_data().",
         call. = FALSE)

  family <- match.arg(family, c("poisson", "nb"))
  decay  <- match.arg(decay, c("exponential", "power", "none"))

  obj <- make_model_object(dat, family = family, decay = decay,
                           estimate_decay = estimate_decay,
                           decay_init = decay_init)

  message("Fitting model (Could take a while)...")
  ptm <- proc.time()
  fit <- stats::nlminb(obj$par, obj$fn, obj$gr,
                       control = list(iter.max = 300, eval.max = 300))
  ptm2 <- proc.time()

  if (time) print(ptm2 - ptm)

  # --- Convergence diagnostics ------------------------------------------------
  max_grad <- max(abs(obj$gr(fit$par)))

  if (fit$convergence != 0)
    warning("nlminb did not converge (code ", fit$convergence, "): ",
            fit$message, call. = FALSE)

  if (max_grad > 0.01)
    warning("Large gradient at optimum (max |grad| = ", signif(max_grad, 3),
            "). Results may be unreliable.", call. = FALSE)

  # --- Standard errors via sdreport ------------------------------------------
  message("Computing standard errors via sdreport...")
  sdr <- tryCatch(
    TMB::sdreport(obj),
    error = function(e) {
      warning("sdreport failed: ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )

  if (!is.null(sdr) && !sdr$pdHess)
    warning("Hessian is not positive definite. ",
            "Standard errors may be unreliable.", call. = FALSE)

  # Resolved natural-scale decay parameter (a for power, tau for exponential).
  decay_resolved <- attr(obj, "decay")
  decay_param <- if (decay_resolved == "none") {
    NA_real_
  } else if ("log_decay" %in% names(fit$par)) {
    exp(unname(fit$par["log_decay"]))          # estimated
  } else {
    exp(attr(obj, "log_decay_init"))           # fixed via map
  }

  out <- list(
    obj         = obj,
    fit         = fit,
    sdr         = sdr,
    data        = dat,
    family      = family,
    decay       = decay_resolved,
    decay_param = decay_param
  )
  class(out) <- c("catchment_fit", "list")
  return(out)
}


#' Check convergence of a fitted catchment model
#'
#' Summarises the convergence status, max gradient, and Hessian positive-
#' definiteness of a \code{catchment_fit} object.
#'
#' @param mod A \code{catchment_fit} object from [catchment_model()].
#'
#' @return A named list (invisibly) with elements \code{converged},
#'   \code{nlminb_message}, \code{max_gradient}, and \code{pdHess}.  The
#'   summary is also printed.
#' @export
#'
check_convergence <- function(mod) {

  if (!inherits(mod, "catchment_fit"))
    stop("`mod` must be a catchment_fit object from catchment_model().",
         call. = FALSE)

  max_grad <- max(abs(mod$obj$gr(mod$fit$par)))
  pd_hess  <- if (!is.null(mod$sdr)) mod$sdr$pdHess else NA

  result <- list(
    converged      = mod$fit$convergence == 0,
    nlminb_message = mod$fit$message,
    max_gradient   = max_grad,
    pdHess         = pd_hess
  )

  cat("Convergence summary\n")
  cat(" nlminb converged:", result$converged, "\n")
  cat(" nlminb message  :", result$nlminb_message, "\n")
  cat(" Max |gradient|  :", signif(max_grad, 4), "\n")
  cat(" pdHess          :", if (is.na(pd_hess)) "unknown (sdreport failed)"
                             else as.character(pd_hess), "\n")

  invisible(result)
}


#' Print a catchment_fit object
#' @param x A \code{catchment_fit} object.
#' @param ... Further arguments (currently unused).
#' @return Invisibly returns \code{x}.
#' @method print catchment_fit
#' @export
print.catchment_fit <- function(x, ...) {
  max_grad <- max(abs(x$obj$gr(x$fit$par)))
  pd_hess  <- if (!is.null(x$sdr)) x$sdr$pdHess else NA

  fam_str <- if (!is.null(x$family)) x$family else "poisson"
  cat("catchment_fit\n")
  cat(" Family     :", fam_str, "\n")
  cat(" Facilities :", length(x$data$loc_labels), "\n")
  cat(" Pop pixels :", length(x$data$pop_vec), "\n")
  cat(" Converged  :", x$fit$convergence == 0, "\n")
  cat(" Max |grad| :", signif(max_grad, 3), "\n")
  cat(" pdHess     :", if (is.na(pd_hess)) "unknown" else as.character(pd_hess), "\n")
  invisible(x)
}


#' Constructing model object for catchment model
#'
#' @param dat A list with class \code{catchment_data}.
#' @param family Likelihood family: \code{"poisson"} or \code{"nb"}.
#' @param decay Distance-decay family: \code{"exponential"}, \code{"power"}, or
#'   \code{"none"} (precomputed).  See [catchment_model()].
#' @param estimate_decay TRUE/FALSE: estimate the decay parameter.
#' @param decay_init Initial natural-scale decay parameter (\code{a} for power,
#'   \code{tau} minutes for exponential); \code{NULL} uses the family default.
#'
#' @return A TMB AD function object.  The resolved decay family is attached as
#'   \code{attr(obj, "decay")}.
#' @export
#'
make_model_object <- function(dat, family = "poisson",
                              decay = "exponential", estimate_decay = TRUE,
                              decay_init = NULL) {

  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("Package 'INLA' is required for make_model_object(). Install it from ",
         "<https://www.r-inla.org/download-install>.", call. = FALSE)
  }

  family     <- match.arg(family, c("poisson", "nb"))
  family_int <- if (family == "poisson") 0L else 1L

  # --- Resolve distance-decay configuration ----------------------------------
  decay <- match.arg(decay, c("exponential", "power", "none"))
  has_travel <- !is.null(dat$travel_sparse)
  if (decay != "none" && !has_travel) {
    message("No travel template found in `dat`; falling back to decay = ",
            "\"none\" (precomputed surface). Pass a raw travel matrix to ",
            "prepare_data() to learn the decay parameter.")
    decay <- "none"
  }

  use_cpp_decay <- if (decay == "none") 0L else 1L
  decay_type    <- if (decay == "power") 0L else 1L   # 1 = exponential / unused
  if (is.null(decay_init)) decay_init <- if (decay == "power") 2 else 60
  if (decay_init <= 0)
    stop("`decay_init` must be positive.", call. = FALSE)
  log_decay_init <- log(decay_init)
  # Prior centred on the family default scale, loose SD.
  log_decay_mean <- if (decay == "power") log(2) else log(60)

  alpha   <- 2  # Smoothness parameter (Matern kernel=2)
  nu      <- alpha - 1
  spde    <- (INLA::inla.spde2.matern(mesh = dat$mesh, alpha = alpha)$param.inla)[c("M0", "M1", "M2")]
  A_pixel <- INLA::inla.spde.make.A(mesh = dat$mesh, loc = as.matrix(dat$pixel_coords))
  n_s     <- nrow(spde$M0)

  # Covariate matrices (zero-column = no covariates)
  X_pixel     <- if (!is.null(dat$X_pixel)) dat$X_pixel else matrix(0.0, nrow = length(dat$pop_vec), ncol = 0L)
  Z_hf        <- if (!is.null(dat$Z_hf))   dat$Z_hf    else matrix(0.0, nrow = length(dat$weights),  ncol = 0L)
  n_pixel_cov <- ncol(X_pixel)
  n_fac_cov   <- ncol(Z_hf)

  pixel_hf_probs <- Matrix::Matrix(t(dat$prob_mat_init), sparse = TRUE)

  # Travel template for the C++ decay path. When decay == "none" it is unused in
  # C++; pass the (well-formed) precomputed matrix as a placeholder so TMB can
  # read it (an empty sparse matrix cannot be parsed).
  travel_hf <- if (use_cpp_decay == 1L) dat$travel_sparse else pixel_hf_probs

  input_data <- list(
    Y_hf           = dat$weights,
    spde           = spde,
    A_pixel        = A_pixel,
    pop_pixel      = dat$pop_vec,
    pixel_hf_probs = pixel_hf_probs,
    which_not_NA   = dat$which_not_NA,
    learn_hf_mass  = 1L,
    use_cpp_decay  = use_cpp_decay,
    travel_hf      = travel_hf,
    decay_type     = decay_type,
    log_decay_mean = log_decay_mean,
    log_decay_sd   = 1.0,
    family         = family_int,
    n_pixel_cov    = n_pixel_cov,
    X_pixel        = X_pixel,
    n_fac_cov      = n_fac_cov,
    Z_hf           = Z_hf,
    # Prior parameters
    log_rho_mean     = log(5),
    log_rho_sd       = 0.5,
    log_sigma_mean   = -1,
    log_sigma_sd     = 0.5,
    nu               = nu,
    log_hf_mass_mean = 0.0,
    log_hf_mass_sd   = 0.1,
    log_nb_phi_mean  = 2.0,   # prior centred on phi≈7 (moderate overdispersion)
    log_nb_phi_sd    = 1.0
  )

  parameters <- list(
    beta_0      = 0,
    S           = rep(0, n_s),
    log_rho     = 0,
    log_sigma   = 0,
    log_hf_mass = rep(0, length(dat$weights)),
    beta        = rep(0, n_pixel_cov),
    gamma       = rep(0, n_fac_cov),
    log_nb_phi  = 2.0,            # start at prior mean
    log_decay   = log_decay_init
  )

  # Fix log_nb_phi when using Poisson (not estimated)
  tmb_map <- list()
  if (family_int == 0L)
    tmb_map$log_nb_phi <- factor(NA)

  # Fix log_decay unless an active C++ decay is being estimated
  if (use_cpp_decay == 0L || !estimate_decay)
    tmb_map$log_decay <- factor(NA)

  obj <- TMB::MakeADFun(
    data       = input_data,
    parameters = parameters,
    map        = tmb_map,
    random     = c("S", "log_hf_mass"),
    silent     = FALSE,
    DLL        = "catchment"
  )

  attr(obj, "decay") <- decay
  attr(obj, "log_decay_init") <- log_decay_init
  return(obj)
}
