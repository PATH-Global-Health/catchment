#include <TMB.hpp>
#include <math.h>
#include <stdio.h>

template<class Type>
Type objective_function<Type>::operator() ()
{
  using namespace R_inla;
  using namespace density;
  using namespace Eigen;

  //prevalence stuff
  DATA_VECTOR(Y_hf);
  DATA_STRUCT(spde,spde_t);
  DATA_SPARSE_MATRIX(A_pixel);
  DATA_VECTOR(pop_pixel);
  DATA_SPARSE_MATRIX(pixel_hf_probs); //n_hf by n_pixel
  DATA_IVECTOR(which_not_NA);
  DATA_INTEGER(learn_hf_mass);



  //prior settings
  // DATA_SCALAR(prior_rho_min);
  // DATA_SCALAR(prior_rho_prob);
  // DATA_SCALAR(prior_sigma_max);
  // DATA_SCALAR(prior_sigma_prob);
  DATA_SCALAR(log_rho_mean);
  DATA_SCALAR(log_rho_sd);
  DATA_SCALAR(log_sigma_mean);
  DATA_SCALAR(log_sigma_sd);
  DATA_SCALAR(nu);
  DATA_SCALAR(log_hf_mass_mean);
  DATA_SCALAR(log_hf_mass_sd);


  PARAMETER(beta_0);
  PARAMETER_VECTOR(S);
  PARAMETER(log_rho);
  PARAMETER(log_sigma);
  PARAMETER_VECTOR(log_hf_mass);


  Type f=0;

  int n_pixel = pixel_hf_probs.cols();
  int n_hf = pixel_hf_probs.rows();

  Type sigma = exp(log_sigma);
  Type rho = exp(log_rho);
  Type kappa = sqrt(8.0) / rho;
  SparseMatrix<Type> Q = Q_spde(spde, kappa);
  Type scaling_factor = sqrt(exp(lgamma(nu)) / (exp(lgamma(nu + 1)) * 4 * M_PI * pow(kappa, 2*nu)));
  // Type lambdatilde1 = -log(prior_rho_prob) * prior_rho_min;
  // Type lambdatilde2 = -log(prior_sigma_prob) / prior_sigma_max;
  // Type log_pcdensity = log(lambdatilde1) + log(lambdatilde2) - 2 * log_rho
  //   -lambdatilde1 * pow(rho, -1) - lambdatilde2 * sigma;
  // f -= log_pcdensity + log_rho + log_sigma ;


  f -= dnorm(log_rho, log_rho_mean, log_rho_sd, true);
  f -= dnorm(log_sigma, log_sigma_mean, log_sigma_sd, true);



  vector<Type> pops_hf(n_hf);
  vector<Type> log_rate_pixel =  A_pixel * S;

  for(int i=0; i<n_hf; i++){
    f -= dnorm(log_hf_mass(i), log_hf_mass_mean, log_hf_mass_sd, true);
  }



  Type prob_sum;
  //modify probabilities using weights
  for(int i=0; i<n_hf; i++){
    pixel_hf_probs.row(i) *= exp(log_hf_mass(i));
  }

  //make sure probs sum to 1
  for(int i=0; i<n_pixel; i++){
    pixel_hf_probs.col(i) /= pixel_hf_probs.col(i).sum();
  }



  Type pixel_pop_contr;
  vector<Type> case_vector;

  case_vector = exp(log_rate_pixel + beta_0) * pop_pixel;
  vector<Type> case_hf = pixel_hf_probs * case_vector;
  vector<Type> pop_hf = pixel_hf_probs * pop_pixel;


  for(int i=0; i<n_hf; i++){
    if(which_not_NA(i)){
      if(case_hf(i) > 0){
        f -= dpois(Y_hf(i), case_hf(i), true);
      }else{
        std::cout<<i<<"\n";
      }
    }
  }



  f += SCALE(GMRF(Q), sigma / scaling_factor)(S);

  REPORT(case_hf);
  REPORT(pop_hf);
  return(f);

}
