#include <TMB.hpp>
#include <math.h>
#include <stdio.h>

template<class Type>
Type objective_function<Type>::operator() ()
{
  using namespace R_inla;
  using namespace density;
  using namespace Eigen;

  DATA_VECTOR(Y_hf);
  DATA_STRUCT(spde, spde_t);
  DATA_SPARSE_MATRIX(A_pixel);
  DATA_VECTOR(pop_pixel);
  DATA_SPARSE_MATRIX(pixel_hf_probs);   // n_hf × n_pixel (legacy precomputed-decay path)
  DATA_IVECTOR(which_not_NA);
  DATA_INTEGER(learn_hf_mass);

  // Distance-decay: when use_cpp_decay==1 the decay function is applied in C++ to
  // the clamped travel times in travel_hf (kept sparsity = force_threshold +
  // n_fac_limit mask); when 0 the precomputed pixel_hf_probs is used unchanged.
  DATA_INTEGER(use_cpp_decay);
  DATA_SPARSE_MATRIX(travel_hf);        // n_hf × n_pixel clamped travel times
  DATA_INTEGER(decay_type);             // 0 = power d^-a, 1 = exponential exp(-d/tau)
  DATA_SCALAR(log_decay_mean);
  DATA_SCALAR(log_decay_sd);

  // Likelihood family: 0 = Poisson (default), 1 = Negative Binomial
  DATA_INTEGER(family);

  // Pixel-level covariates [n_pixel × n_pixel_cov]; n_pixel_cov==0 → no-op
  DATA_INTEGER(n_pixel_cov);
  DATA_MATRIX(X_pixel);

  // Facility-level covariates [n_hf × n_fac_cov]; n_fac_cov==0 → no-op
  DATA_INTEGER(n_fac_cov);
  DATA_MATRIX(Z_hf);

  // Prior settings
  DATA_SCALAR(log_rho_mean);
  DATA_SCALAR(log_rho_sd);
  DATA_SCALAR(log_sigma_mean);
  DATA_SCALAR(log_sigma_sd);
  DATA_SCALAR(nu);
  DATA_SCALAR(log_hf_mass_mean);
  DATA_SCALAR(log_hf_mass_sd);
  DATA_SCALAR(log_nb_phi_mean);   // prior mean for log NB dispersion
  DATA_SCALAR(log_nb_phi_sd);     // prior SD for log NB dispersion

  PARAMETER(beta_0);
  PARAMETER_VECTOR(S);
  PARAMETER(log_rho);
  PARAMETER(log_sigma);
  PARAMETER_VECTOR(log_hf_mass);
  PARAMETER_VECTOR(beta);      // pixel covariate coefficients [n_pixel_cov]
  PARAMETER_VECTOR(gamma);     // facility covariate coefficients [n_fac_cov]
  PARAMETER(log_nb_phi);       // log NB size/dispersion (mapped fixed when family==0)
  PARAMETER(log_decay);        // log decay param (a or tau; mapped fixed when not learned)

  Type f = 0;

  int n_hf    = Y_hf.size();
  int n_pixel = pop_pixel.size();

  Type sigma = exp(log_sigma);
  Type rho   = exp(log_rho);
  Type kappa = sqrt(8.0) / rho;
  SparseMatrix<Type> Q = Q_spde(spde, kappa);
  Type scaling_factor = sqrt(exp(lgamma(nu)) /
    (exp(lgamma(nu + 1)) * 4 * M_PI * pow(kappa, 2 * nu)));

  f -= dnorm(log_rho,   log_rho_mean,   log_rho_sd,   true);
  f -= dnorm(log_sigma, log_sigma_mean, log_sigma_sd, true);

  // NB dispersion prior (only contributes when family == 1)
  if(family == 1){
    f -= dnorm(log_nb_phi, log_nb_phi_mean, log_nb_phi_sd, true);
  }

  // Per-facility mean for log_hf_mass prior: log_hf_mass_mean + Z_hf * gamma
  vector<Type> hf_mass_mean(n_hf);
  for(int i = 0; i < n_hf; i++) hf_mass_mean(i) = log_hf_mass_mean;
  if(n_fac_cov > 0){
    hf_mass_mean += Z_hf * gamma;
  }

  for(int i = 0; i < n_hf; i++){
    f -= dnorm(log_hf_mass(i), hf_mass_mean(i), log_hf_mass_sd, true);
  }

  // Pixel log-rate: SPDE field + optional pixel covariates
  vector<Type> pops_hf(n_hf);
  vector<Type> log_rate_pixel = A_pixel * S;
  if(n_pixel_cov > 0){
    log_rate_pixel += X_pixel * beta;
  }

  // Build the working probability matrix [n_hf × n_pixel].  Either apply the
  // decay function to the clamped travel times (kept sparsity preserved), or use
  // the precomputed legacy matrix.  The per-pixel column-normalisation below
  // makes the result invariant to any per-pixel scaling, so a power decay with
  // exponent 2 reproduces the legacy 1/d^2 surface exactly.
  SparseMatrix<Type> probs;
  if(use_cpp_decay == 1){
    probs = travel_hf;
    Type dpar = exp(log_decay);
    for(int k = 0; k < probs.outerSize(); ++k){
      for(typename SparseMatrix<Type>::InnerIterator it(probs, k); it; ++it){
        Type d = it.value();
        if(decay_type == 0){
          it.valueRef() = pow(d, -dpar);     // power: d^-a
        } else {
          it.valueRef() = exp(-d / dpar);    // exponential: exp(-d/tau)
        }
      }
    }
    f -= dnorm(log_decay, log_decay_mean, log_decay_sd, true);
  } else {
    probs = pixel_hf_probs;
  }

  // Re-weight probability matrix by HF mass, then column-normalise
  for(int i = 0; i < n_hf; i++){
    probs.row(i) *= exp(log_hf_mass(i));
  }
  for(int i = 0; i < n_pixel; i++){
    probs.col(i) /= probs.col(i).sum();
  }

  vector<Type> case_vector = exp(log_rate_pixel + beta_0) * pop_pixel;
  vector<Type> case_hf = probs * case_vector;
  vector<Type> pop_hf  = probs * pop_pixel;

  // Likelihood
  for(int i = 0; i < n_hf; i++){
    if(which_not_NA(i)){
      if(case_hf(i) > 0){
        if(family == 0){
          f -= dpois(Y_hf(i), case_hf(i), true);
        } else {
          // dnbinom_robust(x, log_mu, log_phi_size, give_log)
          f -= dnbinom_robust(Y_hf(i), log(case_hf(i)), log_nb_phi, true);
        }
      }
    }
  }

  f += SCALE(GMRF(Q), sigma / scaling_factor)(S);

  REPORT(case_hf);
  REPORT(pop_hf);
  ADREPORT(case_hf);
  ADREPORT(pop_hf);
  return f;
}
