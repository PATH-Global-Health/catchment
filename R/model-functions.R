

#' Fit catchment model
#'
#' Fits the gravity-style catchment model and computes parameter uncertainty
#' via [TMB::sdreport()].  Convergence is checked automatically and warnings are
#' issued when the optimiser reports failure, the max gradient is large, or the
#' Hessian is not positive-definite.
#'
#' @param dat A [catchment_data] object created by [prepare_data()].
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
catchment_model <- function(dat, time = TRUE) {

  if (!inherits(dat, "catchment_data"))
    stop("`dat` must be a catchment_data object created by prepare_data().",
         call. = FALSE)

  obj <- make_model_object(dat)

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

  out <- list(
    obj  = obj,
    fit  = fit,
    sdr  = sdr,
    data = dat
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

  cat("catchment_fit\n")
  cat(" Facilities :", length(x$data$loc_labels), "\n")
  cat(" Pop pixels :", length(x$data$pop_vec), "\n")
  cat(" Converged  :", x$fit$convergence == 0, "\n")
  cat(" Max |grad| :", signif(max_grad, 3), "\n")
  cat(" pdHess     :", if (is.na(pd_hess)) "unknown" else as.character(pd_hess), "\n")
  invisible(x)
}


#' Constructing model object for catchment model
#'
#' @param dat A list with class(catchment_data)
#'
#' @return A TMB list object
#' @export
#'
make_model_object <- function(dat) {

  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("Package 'INLA' is required for make_model_object(). Install it from ",
         "<https://www.r-inla.org/download-install>.", call. = FALSE)
  }

  alpha <- 2  # Smoothness parameter (Matern kernel=2)
  nu <- alpha - 1
  spde <- (INLA::inla.spde2.matern(mesh = dat$mesh, alpha = alpha)$param.inla)[c("M0", "M1", "M2")]
  A_pixel <- INLA::inla.spde.make.A(mesh = dat$mesh, loc = as.matrix(dat$pixel_coords))
  n_s <- nrow(spde$M0)

  input_data <- list(
    Y_hf = dat$weights,
    spde = spde,
    A_pixel = A_pixel,
    pop_pixel = dat$pop_vec,
    pixel_hf_probs = Matrix::Matrix(t(dat$prob_mat_init), sparse = TRUE),
    which_not_NA = dat$which_not_NA,
    learn_hf_mass = 1L,
    # Prior parameters
    log_rho_mean = log(5),
    log_rho_sd = 0.5,
    log_sigma_mean = -1,
    log_sigma_sd = 0.5,
    nu = nu,
    log_hf_mass_mean = 0.0,
    log_hf_mass_sd = 0.1
  )

  parameters <- list(
    beta_0       = 0,
    S            = rep(0, n_s),
    log_rho      = 0,
    log_sigma    = 0,
    log_hf_mass  = rep(0, length(dat$weights))
  )

  obj <- TMB::MakeADFun(
    data       = input_data,
    parameters = parameters,
    random     = c("S", "log_hf_mass"),
    silent     = FALSE,
    DLL        = "catchment"
  )

  return(obj)
}
