// // -- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; --


// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>


// [[Rcpp::plugins(openmp)]]

#ifdef _OPENMP
#include <omp.h>
#endif

// [[Rcpp::plugins(cpp11)]]
#include <random>

using namespace Rcpp;




// [[Rcpp::export]]
SEXP summaryC(NumericMatrix x,
              int nrow, int ncol, const int ncores)
{
  NumericMatrix result(nrow, 5);
  int indices[6] = {0, 1,  250,  501,  751, 1000};
  
  //   #pragma omp parallel num_threads(ncores)
  {
    //     #pragma omp for
    for(int i = 0; i < nrow; i++){
      double total = 0;
      double totalsquare = 0;
      for (int j = 0; j < ncol; j++){
        total += x(i,j);
        totalsquare += pow(x(i,j),2);
      }
      NumericVector v = (x.row(i));
      for(int q=0; q<5; q++){ 
        std::nth_element(v.begin() + indices[q] + 1, v.begin() + indices[q+1], v.end());
        result(i,q+3) = *(v.begin() + indices[q+1]);
      }
    }
  }
  return Rcpp::wrap(result);
}


// [[Rcpp::export]]
SEXP summaryParC(NumericMatrix x,
                 int nrow, int ncol, const int ncores)
{
  NumericMatrix result(nrow, 5);
  int indices[6] = {0, 1,  250,  501,  751, 1000};
  
  #pragma omp parallel num_threads(ncores)
  {
    #pragma omp for ordered schedule(dynamic)
      for(int i = 0; i < nrow; i++){
        #pragma omp ordered
        {
          NumericVector v = (x.row(i));
          for(int q=0; q<5; q++){ 
          std::nth_element(v.begin() + indices[q] + 1, v.begin() + indices[q+1], v.end());
          result(i,q+3) = *(v.begin() + indices[q+1]);
          }
        }
      } 
  }
  return Rcpp::wrap(result);
}
