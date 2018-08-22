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
// SEXP matrixmultiplicationC(const Eigen::Map<Eigen::MatrixXd> A, const Eigen::Map<Eigen::MatrixXd> B){
//   Eigen::MatrixXd C = A * B;
//   
//   return Rcpp::wrap(C);
// }



// [[Rcpp::export]]
SEXP rowmeanC(NumericMatrix x) {
  int nrow = x.nrow(), ncol = x.ncol();
  NumericVector out(nrow);
  
  for (int i = 0; i < nrow; i++) {
    double total = 0;
    for (int j = 0; j < ncol; j++) {
      total += x(i, j);
    }
    out[i] = total / ncol;
  }
  return Rcpp::wrap(out);
}





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

SEXP summaryC(const Eigen::Map<Eigen::MatrixXd> x, 
              const int nrow, const int ncol,
              const Eigen::Map<Eigen::VectorXd> quantiles)
{
  Eigen::MatrixXd result;

  for(int i = 0; i < nrow; i++){
    double total = 0;
    for (int j = 0, j < ncol; j++){
      total += x(i,j);
      result(i,1)
      result(i,0)
      result(i,0)
      result(i,0)
    }
    result(i,0) = total / ncol;
    
  }
  
  return Rcpp::wrap(result);
}






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









// 
// // [[Rcpp::export]]
// NumericMatrix rddrawmatrixC(int num_unique_elements, int n_bootstrap,
//                             NumericVector elements_to_draw_from, int n_obs_censusdata)
// {
//   // construct a trivial random generator engine from a time-based seed:
//   unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
//   std::default_random_engine generator (seed);
//   
//   // make a uniform distribution with lower limit 1 and upper limit number of locations
//   // this represents the indices that we want
//   std::uniform_int_distribution<int> distribution(1, num_unique_elements);
//   
//   // initialise matrix that can be filled
//   NumericMatrix returnmatrix(n_obs_censusdata, n_bootstrap);
//   int matrixsize = n_obs_censusdata * n_bootstrap;
//   
//   for (int i=0; i<matrixsize; ++i)
//     returnmatrix[i] = elements_to_draw_from[distribution(generator)-1];
//   // subtract 1 because in C++ indices start with 0
//   
//   return returnmatrix;
// }
// 
// 
// 
// 
// // [[Rcpp::export]]
// SEXP rddrawmatrixC2(const int n_bootstrap,
//                     const Eigen::Map<Eigen::VectorXd> elements_to_draw_from, 
//                     const int n_obs_censusdata)
// {
//   const int upper = elements_to_draw_from.size();
//   std::random_device rd; // used to obtain a seed for the number engine
//   std::mt19937 gen(rd()); // Mersenne Twister engine 
//   std::uniform_int_distribution<> dis(1, upper);
//   
//   // initialise matrix that can be filled
//   NumericMatrix returnmatrix(n_obs_censusdata, n_bootstrap);
//   const int matrixsize = n_obs_censusdata * n_bootstrap;
//   
//   for (int i=0; i<matrixsize; ++i)
//     returnmatrix[i] = elements_to_draw_from[dis(gen)-1]; // subtract 1 because in C++ indices start with 0
//   
//   return Rcpp::wrap(returnmatrix);
// }



// // and we can use Rcpp::List to return both at the same time
// //
// // [[Rcpp::export]]
// Rcpp::List rcppeigen_bothproducts(const Eigen::VectorXd & x) {
//     Eigen::MatrixXd op = x * x.transpose();
//     double          ip = x.transpose() * x;
//     return Rcpp::List::create(Rcpp::Named("outer")=op,
//                               Rcpp::Named("inner")=ip);
// }





// Rcpp::sourceCpp("C:/Users/nikos/Desktop/ELLsae/src/ELLsaeCpp.cpp")



