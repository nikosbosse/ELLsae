// // -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-


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
              NumericVector quantiles,
              int nrow, int ncol, const int ncores)
{
  const int no_quantiles = quantiles.size();
  NumericMatrix result(nrow, no_quantiles + 3);
  int indices[no_quantiles +1];
  indices[0] = -1;
  for (int k=0; k<no_quantiles; k++){
    if (quantiles[k] < 0.5){
      indices[k+1] = floor(quantiles[k] * (ncol-1));
    } else {
      indices[k+1] = ceil(quantiles[k] * (ncol-1));
    }
  }
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





// [[Rcpp::export]]
SEXP summaryParC(NumericMatrix x,
              NumericVector quantiles,
              int nrow, int ncol, const int ncores)
{
  const int no_quantiles = quantiles.size();
  NumericMatrix result(nrow, no_quantiles + 3);
  int indices[no_quantiles +1];
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
// 
// 
// // [[Rcpp::export]]
// SEXP summaryParC(const RcppParallel::RMatrix x,
//                  const RcppParallel::RVector quantiles)
// {
//   const int no_quantiles = quantiles.size();
//   nrow = x.nrow();
//   ncol = x.ncol();
//   RcppParallel::RMatrix result(nrow, no_quantiles + 3);
//   
//   // IntegerVector quant_ind_round_up(no_quantiles);
//   // IntegerVector quant_ind_round_down(no_quantiles);
//   // for (int k=0; k<no_quantiles; k++){
//   //   quant_ind_round_up[k] = std::fmax(ceil(quantiles[k]*ncol) -1, 0);
//   //   quant_ind_round_down[k] = std::fmax(floor(quantiles[k]*ncol)-1, 0);
//   // }
//   
//   RcppParallel::RVector indices(no_quantiles);
//   indices[0] = -1;
//   for (int k=0; k<no_quantiles; k++){
//     if (quantiles[k] < 0.5){
//       indices[k+1] = floor(quantiles[k] * (ncol-1));
//     } else {
//       indices[k+1] = ceil(quantiles[k] * (ncol-1));
//     }
//   }
//   
//   
// #ifdef _OPENMP
//   const int ncores = omp_get_max_threads() - 2;
//   
// #pragma omp parallel num_threads(ncores)
// {
//   
// #pragma omp for
//   for(int i = 0; i < nrow; i++){
//     double total = 0;
//     double totalsquare = 0;
//     
// // #pragma omp simd
//     for (int j = 0; j < ncol; j++){
//       total += x(i,j);
//       totalsquare += pow(x(i,j),2);
//     }
//     result(i,0) = total / ncol; //mean
//     result(i,1) = totalsquare / ncol - pow(result(i,0),2); //var
//     result(i,2) = sqrt(result(i,1)); //sd
//     
//     RcppParallel::Rvector v = (x.row(i));
//     for(int q=0; q<no_quantiles; q++){ //quantiles
//       std::nth_element(v.begin() + indices[q] + 1, v.begin() + indices[q+1], v.end());
//       result(i,q+3) = *(std::begin(v) + indices[q+1]);
//     }
//     
//   }
// }
// 
// #else 
// for(int i = 0; i < nrow; i++){
//   double total = 0;
//   double totalsquare = 0;
//   for (int j = 0; j < ncol; j++){
//     total += x(i,j);
//     totalsquare += pow(x(i,j),2);
//   }
//   result(i,0) = total / ncol; //mean
//   result(i,1) = totalsquare / ncol - pow(result(i,0),2); //var
//   result(i,2) = sqrt(result(i,1)); //sd
//   
//   RcppParallel::RVector v = (x.row(i));
//   for(int q=0; q<no_quantiles; q++){ //quantiles
//     std::nth_element(v.begin() + indices[q] + 1, v.begin() + indices[q+1], v.end());
//     result(i,q+3) = *(std::begin(v) + indices[q+1]);
//   }
//   
// }
// 
// #endif
// return Rcpp::wrap(result);
// 
// }
// 
// 
// 

// // [[Rcpp::export]]
// SEXP summaryC(const NumericMatrix& x,
//               const NumericVector& quantiles)
// {
//   int no_quantiles = quantiles.size();
//   int nrow = x.nrow();
//   int ncol = x.ncol();
//   Eigen::MatrixXd result(nrow, no_quantiles + 3);
//   
//   // IntegerVector quant_ind_round_up(no_quantiles);
//   // IntegerVector quant_ind_round_down(no_quantiles);
//   // for (int k=0; k<no_quantiles; k++){
//   //   quant_ind_round_up[k] = std::fmax(ceil(quantiles[k]*ncol) -1, 0);
//   //   quant_ind_round_down[k] = std::fmax(floor(quantiles[k]*ncol)-1, 0);
//   // }
//   
//   int indices[no_quantiles];
//   indices[0] = -1;
//   for (int k=0; k<no_quantiles; k++){
//     if (quantiles[k] < 0.5){
//       indices[k+1] = floor(quantiles[k] * (ncol-1));
//     } else {
//       indices[k+1] = ceil(quantiles[k] * (ncol-1));
//     }
//   }
// 
//   
//   for(int i = 0; i < nrow; i++){
//     double total = 0;
//     double totalsquare = 0;
//     for (int j = 0; j < ncol; j++){
//       total += x(i,j);
//       totalsquare += pow(x(i,j),2);
//     }
//     result(i,0) = total / ncol; //mean
//     result(i,1) = totalsquare / ncol - pow(result(i,0),2); //var
//     result(i,2) = sqrt(result(i,1)); //sd
//     
//     NumericVector v = (x.row(i));
//     for(int q=0; q<no_quantiles; q++){ //quantiles
//       std::nth_element(v.begin() + indices[q] + 1, v.begin() + indices[q+1], v.end());
//       result(i,q+3) = *(std::begin(v) + indices[q+1]);
//     }
//     
//   }
//   // NumericVector a = (x.row(1));
//   // a = a.sort();
//   // return Rcpp::wrap(a);
//   return Rcpp::wrap(result);
// 
// }

// 
// 
// accepted
//   Instead of doing std::sort(quantile.begin(), quantile.end()) a somewhat cheaper way would be
//   
//   auto const Q1 = quantile.size() / 4;
// auto const Q2 = quantile.size() / 2;
// auto const Q3 = Q1 + Q2;
// 
// std::nth_element(quantile.begin(),          quantile.begin() + Q1, quantile.end());
// std::nth_element(quantile.begin() + Q1 + 1, quantile.begin() + Q2, quantile.end());
// std::nth_element(quantile.begin() + Q2 + 1, quantile.begin() + Q3, quantile.end());
// 
// 



// 
// NumericVector y = x(_,j); // Copy column -- original will not be mod
// std::nth_element(y.begin(), y.begin() + position, y.end())
//   
// 
//   
//   Rcpp::sourceCpp("C:/Users/nikos/Desktop/ELLsae/src/SummaryC.cpp")