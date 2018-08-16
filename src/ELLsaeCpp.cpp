// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppEigen.h which pulls Rcpp.h in for us


#include <RcppEigen.h>
#include <chrono>
#include <random>

using namespace Rcpp;
// via the depends attribute we tell Rcpp to create hooks for
// RcppEigen so that the build process will know what to do
//
// [[Rcpp::depends(RcppEigen)]]

// via the exports attribute we tell Rcpp to make this function
// available from R
//


// // and we can use Rcpp::List to return both at the same time
// //
// // [[Rcpp::export]]
// Rcpp::List rcppeigen_bothproducts(const Eigen::VectorXd & x) {
//     Eigen::MatrixXd op = x * x.transpose();
//     double          ip = x.transpose() * x;
//     return Rcpp::List::create(Rcpp::Named("outer")=op,
//                               Rcpp::Named("inner")=ip);
// }



// [[Rcpp::export]]
SEXP matrixmultiplicationC(const Eigen::Map<Eigen::MatrixXd> A, Eigen::Map<Eigen::MatrixXd> B){
  Eigen::MatrixXd C = A * B;
  
  return Rcpp::wrap(C);
}



// [[Rcpp::export]]
NumericVector rowmeanC(NumericMatrix x) {
  int nrow = x.nrow(), ncol = x.ncol();
  NumericVector out(nrow);
  
  for (int i = 0; i < nrow; i++) {
    double total = 0;
    for (int j = 0; j < ncol; j++) {
      total += x(i, j);
    }
    out[i] = total / ncol;
  }
  return out;
}



// [[Rcpp::export]]
NumericMatrix rddrawmatrixC(int num_unique_elements, int n_bootstrap,
                            NumericVector elements_to_draw_from, int n_obs_censusdata)
{
  // construct a trivial random generator engine from a time-based seed:
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
  std::default_random_engine generator (seed);
  
  // make a uniform distribution with lower limit 1 and upper limit number of locations
  // this represents the indices that we want
  std::uniform_int_distribution<int> distribution(1, num_unique_elements);
  
  // initialise matrix that can be filled
  NumericMatrix returnmatrix(n_obs_censusdata, n_bootstrap);
  int matrixsize = n_obs_censusdata * n_bootstrap;
  
  for (int i=0; i<matrixsize; ++i)
    returnmatrix[i] = elements_to_draw_from[distribution(generator)-1];
  // subtract 1 because in C++ indices start with 0
  
  return returnmatrix;
}

// Rcpp::sourceCpp("C:/Users/nikos/Desktop/ELLsae/src/SamplingWithC.cpp")







