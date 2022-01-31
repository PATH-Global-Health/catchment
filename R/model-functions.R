



#' Fit catchment model
#'
#' @param dat A list created by prepare_data()
#' @param time TRUE/FALSE Print run time?
#'
#' @return A list from model output
#' @export
#'
catchment_model <- function(dat, time = T) {

  obj <- make_model_object(dat)

  message("Fitting model (Could take a while)...")
  ptm <- proc.time()
  # nlminb is a general optimization
  fit <- stats::nlminb(obj$par, obj$fn, obj$gr, control=list(iter.max=300,eval.max=300))
  ptm2 <- proc.time()

  if(time){print(ptm2 - ptm)}

  out <- list(
    obj = obj,
    fit = fit,
    data = dat
  )

  return(out)
}





#' Constructing model object for catchment model
#'
#' @param dat A list with class(catchment_data)
#'
#' @return A TMB list object
#' @export
#'

make_model_object <- function(dat) {

  alpha <- 2  # Smoothness parameter (Matern kernel=2)
  nu <- alpha - 1
  spde <- (INLA::inla.spde2.matern(mesh=dat$mesh, alpha=alpha)$param.inla)[c("M0","M1","M2")]
  A_pixel <- INLA::inla.spde.make.A(mesh=dat$mesh, loc=as.matrix(dat$pixel_coords))  # This goes from values on GP mesh nodes to pixel locations (linear interpolate)
  n_s <- nrow(spde$M0)  # Number of nodes, check for estimating model size
  mesh_coords <- dat$mesh$loc[, 1:2]  # Node locations

  # if("access_mat" %in% class(prob_mat_init) & !"Matrix" %in% class(prob_mat_init)) {
  #   class(prob_mat_init) <- class(prob_mat_init)[!class(prob_mat)%in%"access_mat"]
  #   prob_mat_init <- Matrix::Matrix(t(prob_mat_init), sparse = T)}


  # Passing values to TMB model
  # m <- TMB::MakeADFun(
  #   data = list(
  #     Y_hf = dat$weights,
  #     spde = spde,
  #     A_pixel = A_pixel,
  #     pop_pixel = dat$pop_vec,  # vector of populations
  #     pixel_hf_probs = dat$prob_mat_init,
  #     which_not_NA = dat$which_not_NA,  # Skip NAs
  #     learn_hf_mass = 1,  # 1 means TRUE for learning HF masses
  #     # Prior parameters
  #     log_rho_mean = log(5),  # Rho is the range parameters (distance where correlation is 0.1), smaller for smaller area
  #     log_rho_sd = 0.5,
  #     log_sigma_mean = -1,  # Sigma is the marginal variance (how big the field can get)
  #     log_sigma_sd = 0.5,
  #     nu = nu,  # Smoothness
  #     log_hf_mass_mean = 0.0,  # Mean for HF masses (1 suggests everywhere is the same)
  #     log_hf_mass_sd = 0.1  # May want to do some prior predictive testing here
  #   ),
  #   # Starting values
  #   parameters = list(
  #     beta_0=runif(1, -1, 1),
  #     S=rep(0, n_s),  # Field values on the mesh (vector for static, matrix for dynamic)
  #     # beta=rnorm(n_covs),
  #     log_rho=0,
  #     log_sigma=0,
  #     log_hf_mass = rep(0,  length(dat$weight))),
  #   random = c("S", "log_hf_mass"),  # Integrate our random effects
  #   DLL = "static",
  #   silent=silent
  # )

  input_data <- list(
    Y_hf = dat$weights,
    spde = spde,
    A_pixel = A_pixel,
    pop_pixel = dat$pop_vec,  # vector of populations
    pixel_hf_probs = Matrix::Matrix(t(dat$prob_mat_init), sparse = T),
    which_not_NA = dat$which_not_NA,  # Skip NAs
    learn_hf_mass = 1,  # 1 means TRUE for learning HF masses
    # Prior parameters
    log_rho_mean = log(5),  # Rho is the range parameters (distance where correlation is 0.1), smaller for smaller area
    log_rho_sd = 0.5,
    log_sigma_mean = -1,  # Sigma is the marginal variance (how big the field can get)
    log_sigma_sd = 0.5,
    nu = nu,  # Smoothness
    log_hf_mass_mean = 0.0,  # Mean for HF masses (1 suggests everywhere is the same)
    log_hf_mass_sd = 0.1  # May want to do some prior predictive testing here
  )

  parameters <-  list(
    beta_0=runif(1, -1, 1),
    S=rep(0, n_s),  # Field values on the mesh (vector for static, matrix for dynamic)
    # beta=rnorm(n_covs),
    log_rho=0,
    log_sigma=0,
    log_hf_mass = rep(0,  length(dat$weight)))

  obj <- TMB::MakeADFun(
    data = input_data,
    parameters = parameters,
    # map = tmb_map,
    random = c("S", "log_hf_mass"),  # Integrate our random effects
    silent = FALSE,
    DLL = "catchment")

  return(obj)



  }




