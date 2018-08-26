// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
// [[Rcpp::depends(dqrng)]]
#include <xoshiro.h>
// [[Rcpp::plugins(openmp)]]
#ifdef _OPENMP
#include <omp.h>
#endif

// [[Rcpp::plugins(cpp11)]]
#include <random>

using namespace Rcpp;

// [[Rcpp::export]]
SEXP summaryParC(Eigen::MatrixXd x,
                 const Eigen::VectorXd quantiles,
                 int nrow, int ncol, const int ncores)
{
  const int no_quantiles = quantiles.size();
  Eigen::MatrixXd result(nrow, no_quantiles);
  Eigen::VectorXi indices(no_quantiles);
  indices[0] = -1;
  for (int k=1; k<no_quantiles +1; k++){
    if (quantiles[k] < 0.5){
      indices[k+1] = floor(quantiles[k] * (ncol-1));
    } else {
      indices[k+1] = ceil(quantiles[k] * (ncol-1));
    }
  }

#pragma omp parallel num_threads(ncores)
{
#pragma omp for
  for(int i = 0; i < nrow; i++){
    Eigen::VectorXd v = (x.row(i));
    double * B = v.data();
    double * E = B + nrow;

    for(int q=0; q<no_quantiles; q++){ //quantiles
      std::nth_element(B + indices[q] + 1, B + indices[q+1], E);
      result(i,q) = *(B + indices[q+1]);
    }
  }
}
return Rcpp::wrap(result);
}



// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// 
// [[Rcpp::export]]
// SEXP summaryC(NumericMatrix x,
//                  NumericVector quantiles,
//                  int nrow, int ncol, const int ncores)
// {
//   const int no_quantiles = quantiles.size();
//   NumericMatrix result(nrow, no_quantiles);
//   int indices[no_quantiles];
//   indices[0] = -1;
//   for (int k=0; k<no_quantiles; k++){
//     if (quantiles[k] < 0.5){
//       indices[k+1] = floor(quantiles[k] * (ncol-1));
//     } else {
//       indices[k+1] = ceil(quantiles[k] * (ncol-1));
//     }
//   }
// #pragma omp parallel num_threads(ncores)
// {
// #pragma omp for
//   for(int i = 0; i < nrow; i++){
//     NumericVector v = (x.row(i));
//     for(int q=0; q<no_quantiles; q++){ //quantiles
//       std::nth_element(v.begin() + indices[q] + 1, v.begin() + indices[q+1], v.end());
//       result(i,q) = *(v.begin() + indices[q+1]);
//     }
//   }
// }
//   return Rcpp::wrap(result);
// }
// 
// 
// // Rcpp::sourceCpp("C:/Users/nikos/Desktop/ELLsae/src/ELLsae_stack.cpp")