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
SEXP summaryParC(const Eigen::Map<Eigen::MatrixXd> x,
                 const Eigen::Map<Eigen::VectorXd> quantiles,
                 int nrow, int ncol, const int ncores)
{
  const int no_quantiles = quantiles.size();
  Eigen::MatrixXd result(nrow, no_quantiles);
  Eigen::VectorXi indices(no_quantiles +1);
  indices[0] = -1;
  for (int k=0; k<no_quantiles; k++){
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




//   Rcpp::sourceCpp("C:/Users/nikos/Desktop/ELLsae/src/SummaryC2.cpp")