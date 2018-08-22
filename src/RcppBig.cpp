// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(BH, bigstatsr, RcppEigen)]]
#include <RcppEigen.h>
#include <bigstatsr/BMAcc.h>
using namespace Rcpp;


// // [[Rcpp::export]]
// void test(const Eigen::Map<Eigen::MatrixXd> X,
//               Environment fbm)
// {
//   XPtr<FBM> xpMat = fbm["address"];
//   BMAcc<double> macc(xpMat);
//   macc(1,5) = X(5,2);
// }



// [[Rcpp::export]]
NumericVector rowmeansBigC(Environment fbm) {
  
  XPtr<FBM> xpMat = fbm["address"];
  BMAcc<double> macc(xpMat);
  
  int nrow = macc.nrow();
  int ncol = macc.ncol();
  
  NumericVector out(nrow);
  
  for (int i = 0; i < nrow; i++) {
    double total = 0;
    for (int j = 0; j < ncol; j++) {
      total += macc(i, j);
    }
    out[i] = total / ncol;
  }
  return Rcpp::wrap(out);
}


// 
// // [[Rcpp::export]]
// void inferenceCensusBigC(Environment fbm,
//                          const int n_bootstrap,
//                          const int n_obs_censusdata,
//                          const Eigen::Map<Eigen::VectorXd> locationeffects, 
//                          const Eigen::Map<Eigen::VectorXd> residuals,
//                          const Eigen::Map<Eigen::MatrixXd> X,
//                          const Eigen::Map<Eigen::MatrixXd> beta_sample)
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
//   // set up Pointer and Matrix Accessor to FBM
//   XPtr<FBM> xpMat = fbm["address"];
//   BMAcc<double> macc(xpMat);
//   
//   for (int i=0; i<n_obs_censusdata; ++i)
//     for (int j=0; j<n_bootstrap; j++)
//       macc(i,j) = locationeffects[distrloc(gen)-1] + residuals[distrres(gen)-1] + Xbeta(i,j); 
//   // subtract 1 because in C++ indices start with 0
//   
// }








// Rcpp::sourceCpp("C:/Users/nikos/Desktop/ELLsae/src/RcppBig.cpp")



