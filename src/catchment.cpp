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
  DATA_SPARSE_MATRIX(pixel_hf_probs);   // n_hf × n_pixel
  DATA_IVECTOR(which_not_NA);
  DATA_INTEGER(learn_hf_mass);

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

  Type f = 0;

  int n_pixel = pixel_hf_probs.cols();
  int n_hf    = pixel_hf_probs.rows();

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

  // Re-weight probability matrix by HF mass, then column-normalise
  for(int i = 0; i < n_hf; i++){
    pixel_hf_probs.row(i) *= exp(log_hf_mass(i));
  }
  for(int i = 0; i < n_pixel; i++){
    pixel_hf_probs.col(i) /= pixel_hf_probs.col(i).sum();
  }

  vector<Type> case_vector = exp(log_rate_pixel + beta_0) * pop_pixel;
  vector<Type> case_hf = pixel_hf_probs * case_vector;
  vector<Type> pop_hf  = pixel_hf_probs * pop_pixel;

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
