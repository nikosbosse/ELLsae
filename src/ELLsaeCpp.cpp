// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppEigen.h which pulls Rcpp.h in for us


#include <RcppEigen.h>
// #include <chrono>
#include <random>

using namespace Rcpp;
// via the depends attribute we tell Rcpp to create hooks for
// RcppEigen so that the build process will know what to do
//
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins(cpp11)]]



// // [[Rcpp::export]]
// SEXP rowmeanC(NumericMatrix x) {
//   int nrow = x.nrow(), ncol = x.ncol();
//   NumericVector out(nrow);
// 
//   for (int i = 0; i < nrow; i++) {
//     double total = 0;
//     for (int j = 0; j < ncol; j++) {
//       total += x(i, j);
//     }
//     out[i] = total / ncol;
//   }
//   return Rcpp::wrap(out);
// }




// [[Rcpp::export]]
SEXP inferenceCensusC(const int n_bootstrap,
                      const int n_obs_censusdata,
                      const Eigen::Map<Eigen::VectorXd> locationeffects,
                      const Eigen::Map<Eigen::VectorXd> residuals,
                      const Eigen::Map<Eigen::MatrixXd> X,
                      const Eigen::Map<Eigen::MatrixXd> beta_sample)
{

  // --------- create random sample of locations and of residuals --------- //

  // initialise random seeds
  std::random_device rd; // used to obtain a seed for the number engine
  std::mt19937 gen(rd()); // Mersenne Twister engine

  // initialize distributions for randam locations and residuals
  const int upperlocation = locationeffects.size();
  const int upperresiduals = residuals.size();

  std::uniform_int_distribution<> distrloc(1, upperlocation);
  std::uniform_int_distribution<> distrres(1, upperresiduals);

  // initialize and fill matrix for randam locations and residuals
  Eigen::MatrixXd LocationEffectResiduals(n_obs_censusdata, n_bootstrap);

  for (int i=0; i<n_obs_censusdata; ++i)
    for (int j=0; j<n_bootstrap; j++)
      LocationEffectResiduals(i,j) = locationeffects[distrloc(gen)-1] + residuals[distrres(gen)-1]; // subtract 1 because in C++ indices start with 0

  // ----- create Xbeta ------- //
  Eigen::MatrixXd Xbeta = X * beta_sample;

  // ----- combine results ------- //
  Eigen::MatrixXd returnmatrix = Xbeta + LocationEffectResiduals;

  return Rcpp::wrap(returnmatrix);
}





// [[Rcpp::export]]
SEXP summaryC(NumericMatrix x,
              NumericVector quantiles,
              int nrow, int ncol, const int ncores)
{
  const int no_quantiles = quantiles.size();
  NumericMatrix result(nrow, no_quantiles + 3);
  int indices[no_quantiles];
  indices[0] = -1;
  for (int k=0; k<no_quantiles; k++){
    if (quantiles[k] < 0.5){
      indices[k+1] = floor(quantiles[k] * (ncol-1));
    } else {
      indices[k+1] = ceil(quantiles[k] * (ncol-1));
    }
  }
// #pragma omp parallel num_threads(ncores)
{
// #pragma omp for
  for(int i = 0; i < nrow; i++){
    double total = 0;
    double totalsquare = 0;
    for (int j = 0; j < ncol; j++){
      total += x(i,j);
      totalsquare += pow(x(i,j),2);
    }
    result(i,0) = total / ncol; //mean
    result(i,1) = totalsquare / ncol - pow(result(i,0),2); //var
    result(i,2) = sqrt(result(i,1)); //sd
    
    NumericVector v = (x.row(i));
    for(int q=0; q<no_quantiles; q++){ //quantiles
      std::nth_element(v.begin() + indices[q] + 1, v.begin() + indices[q+1], v.end());
      result(i,q+3) = *(v.begin() + indices[q+1]);
    }
  }
}
  return Rcpp::wrap(result);
}















// 
// // [[Rcpp::export]]
// void inferenceCensusC2(const int n_bootstrap,
//                        const int n_obs_censusdata,
//                        NumericMatrix& y_boot,
//                        const Eigen::Map<Eigen::VectorXd> locationeffects,
//                        const Eigen::Map<Eigen::VectorXd> residuals,
//                        const Eigen::Map<Eigen::MatrixXd> X,
//                        const Eigen::Map<Eigen::MatrixXd> beta_sample)
// {
//
//   // ----- create Xbeta ------- //
//   Eigen::MatrixXd Xbeta = X * beta_sample;
//
//   // --------- create random sample of locations and of residuals --------- //
//
//   // initialise random seeds
//   std::random_device rd; // used to obtain a seed for the number engine
//   std::mt19937 gen(rd()); // Mersenne Twister engine
//
//   // initialize distributions for randam locations and residuals
//   const int upperlocation = locationeffects.size();
//   const int upperresiduals = residuals.size();
//
//   std::uniform_int_distribution<> distrloc(1, upperlocation);
//   std::uniform_int_distribution<> distrres(1, upperresiduals);
//
//   // initialize and fill matrix for randam locations and residuals
//   Eigen::MatrixXd LocationEffectResiduals(n_obs_censusdata, n_bootstrap);
//
//   for (int i=0; i<n_obs_censusdata; ++i)
//     for (int j=0; j<n_bootstrap; j++)
//       y_boot(i,j) = locationeffects[distrloc(gen)-1] + residuals[distrres(gen)-1] + Xbeta(i,j);
//   // subtract 1 because in C++ indices start with 0
//
// }






// Rcpp::sourceCpp("C:/Users/nikos/Desktop/ELLsae/src/ELLsaeCpp.cpp")



