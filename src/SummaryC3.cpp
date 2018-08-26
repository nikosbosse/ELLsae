// // -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

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





// [[Rcpp::export]]
SEXP summaryParC(const Eigen::MatrixXd x,
                 const Eigen::VectorXd quantiles, 
                 int nrow, int ncol, const int ncores)
{
  const int no_quantiles = quantiles.size();
  
  Eigen::MatrixXd result(nrow, no_quantiles + 3);

  
  Eigen::VectorXi indices(no_quantiles);
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
    double total = 0;
    double totalsquare = 0;
    
    for (int j = 0; j < ncol; j++){
      total += x(i,j);
      totalsquare += std::pow(x(i,j),2);
    }
    result(i,0) = total / ncol; //mean
    result(i,1) = totalsquare / ncol - pow(result(i,0),2); //var
    result(i,2) = sqrt(result(i,1)); //sd
    
    Eigen::VectorXd v = (x.row(i));
    auto * ptr = v; // this fails
    for(int q=0; q<no_quantiles; q++){ //quantiles
      std::nth_element(ptr + indices[q] + 1, ptr + indices[q+1], ptr + ncol);
      result(i,q+3) = *(ptr + indices[q+1]);
    }
  }
}

return Rcpp::wrap(result);

}
