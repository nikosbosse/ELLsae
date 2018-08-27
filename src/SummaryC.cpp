// // -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// 
// #include <RcppEigen.h>
// #include <random>
// 
// using namespace Rcpp;
// // via the depends attribute we tell Rcpp to create hooks for
// // RcppEigen so that the build process will know what to do
// //
// // [[Rcpp::depends(RcppEigen)]]
// // [[Rcpp::plugins(cpp11)]]
// 
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
//   NumericMatrix result(nrow, no_quantiles + 4);
//   IntegerVector quant_ind_round_up(no_quantiles);
//   IntegerVector quant_ind_round_down(no_quantiles);
//   for (int k=0; k<no_quantiles; k++){
//     quant_ind_round_up[k] = ceil(quantiles[k]*ncol);
//     quant_ind_round_down[k] = floor(quantiles[k]*ncol);
//   }
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
//     v.sort();
//     result(i,3) = v[0];
//     for(int q=0; q<no_quantiles; q++){ //quantiles
//       result(i,q+3) = (v[quant_ind_round_down[q]] + v[quant_ind_round_up[q]]) / 2;
//     }
//     result(i, 4 + no_quantiles) = v[ncol];
//     
//   }
//   // NumericVector a = (x.row(1));
//   // a = a.sort();
//   // return Rcpp::wrap(a);
//   return Rcpp::wrap(result);
// 
// }
// 
// // 
// // NumericVector y = x(_,j); // Copy column -- original will not be mod
// // std::nth_element(y.begin(), y.begin() + position, y.end())
// //   
// // 
// //   
// //   Rcpp::sourceCpp("C:/Users/nikos/Desktop/ELLsae/src/SummaryC.cpp")