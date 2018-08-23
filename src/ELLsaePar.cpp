// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
// [[Rcpp::depends(dqrng)]]
#include <xoshiro.h>
// [[Rcpp::plugins(openmp)]]
#include <omp.h>
// [[Rcpp::plugins(cpp11)]]
#include <random>


// [[Rcpp::export]]
Eigen::MatrixXd funD(const int n_bootstrap,
                     const int n_obs_censusdata, 
                     const Eigen::Map<Eigen::VectorXd> locationeffects, 
                     const Eigen::Map<Eigen::VectorXd> residuals,
                     const Eigen::Map<Eigen::MatrixXd> X, 
                     const Eigen::Map<Eigen::MatrixXd> beta_sample,
                     int ncores) {
  
  // --------- create random sample of locations and of residuals --------- //
  
  // initialise random seeds 
  std::random_device rd; // used to obtain a seed for the number engine
  dqrng::xoshiro256plus gen(rd());
  
  // initialize distributions for randam locations and residuals
  const int upperlocation = locationeffects.size();
  const int upperresiduals = residuals.size();
  
  // subtract 1 because in C++ indices start with 0
  std::uniform_int_distribution<> distrloc(0, upperlocation - 1);
  std::uniform_int_distribution<> distrres(0, upperresiduals - 1);
  
  // initialize and fill matrix for randam locations and residuals 
  Eigen::MatrixXd LocationEffectResiduals(n_obs_censusdata, n_bootstrap);
  
#pragma omp parallel num_threads(ncores)
{
  dqrng::xoshiro256plus lgen(gen);      // make thread local copy of rng 
  lgen.jump(omp_get_thread_num() + 1);  // advance rng by 1 ... ncores jumps 
  
#pragma omp for
  for (int i=0; i<n_obs_censusdata; ++i)
    for (int j=0; j<n_bootstrap; j++)
      LocationEffectResiduals(i,j) = locationeffects[distrloc(lgen)] + residuals[distrres(lgen)];
}  

// ----- create Xbeta ------- //
Eigen::MatrixXd Xbeta = X * beta_sample;

// ----- combine results ------- //
Eigen::MatrixXd returnmatrix = Xbeta + LocationEffectResiduals;

return returnmatrix;

}







// [[Rcpp::export]]
Eigen::MatrixXd funD2(const int n_bootstrap,
                     const int n_obs_censusdata, 
                     const Eigen::Map<Eigen::VectorXd> locationeffects, 
                     const Eigen::Map<Eigen::VectorXd> residuals,
                     const Eigen::Map<Eigen::MatrixXd> X, 
                     const Eigen::Map<Eigen::MatrixXd> beta_sample,
                     int ncores) {
  
  // --------- create random sample of locations and of residuals --------- //
  
  // initialise random seeds 
  std::random_device rd; // used to obtain a seed for the number engine
  dqrng::xoshiro256plus gen(rd());
  
  // initialize distributions for randam locations and residuals
  const int upperlocation = locationeffects.size();
  const int upperresiduals = residuals.size();
  
  // subtract 1 because in C++ indices start with 0
  std::uniform_int_distribution<> distrloc(0, upperlocation - 1);
  std::uniform_int_distribution<> distrres(0, upperresiduals - 1);
  
  // initialize and fill matrix for randam locations and residuals 
  Eigen::MatrixXd returnmatrix(n_obs_censusdata, n_bootstrap);
  
  // ----- create Xbeta ------- //
  Eigen::MatrixXd Xbeta = X * beta_sample;
  
#pragma omp parallel num_threads(ncores)
{
  dqrng::xoshiro256plus lgen(gen);      // make thread local copy of rng 
  lgen.jump(omp_get_thread_num() + 1);  // advance rng by 1 ... ncores jumps 
  
#pragma omp for
  for (int i=0; i<n_obs_censusdata; ++i)
    for (int j=0; j<n_bootstrap; j++)
      returnmatrix(i,j) = locationeffects[distrloc(lgen)] + residuals[distrres(lgen)] + Xbeta(i,j);
}  


return returnmatrix;
}