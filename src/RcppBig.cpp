// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(BH, bigstatsr, dqrng, RcppEigen)]]

#include <RcppEigen.h>
#include <bigstatsr/BMAcc.h>
#include <random>
#include <xoshiro.h>

#ifdef _OPENMP
#include <omp.h>
#endif

// [[Rcpp::export(.InfCensBigCpp)]]
void InfCensBigCpp(Environment fbm, const int n_bootstrap,
                           const int n_obs_censusdata, 
                           const Eigen::Map<Eigen::VectorXd> locationeffects, 
                           const Eigen::Map<Eigen::VectorXd> residuals,
                           const Eigen::Map<Eigen::MatrixXd> X, 
                           const Eigen::Map<Eigen::MatrixXd> beta_sample, 
                           int userseed, int ncores) 
{
  
  // --------- create random sample of locations and of residuals --------- //
  
  // initialise pseudo random number generator and seed it
  dqrng::xoshiro256plus gen;
  gen.seed(userseed);
  
  // initialize distributions for randam locations and residuals
  const int upperlocation = locationeffects.size();
  const int upperresiduals = residuals.size();
  std::uniform_int_distribution<> distrloc(0, upperlocation - 1);
  std::uniform_int_distribution<> distrres(0, upperresiduals - 1);
  
  // create pointer to File Based Matrix
  XPtr<FBM> xpMat = fbm["address"];
  BMAcc<double> macc(xpMat);
  
  
  // initialize and fill matrix for randam locations and residuals 
  Eigen::MatrixXd LocationEffectResiduals(n_obs_censusdata, n_bootstrap);
  
  
  
#pragma omp parallel num_threads(ncores)
{
  dqrng::xoshiro256plus lgen(gen);      // make thread local copy of rng 
  lgen.jump(omp_get_thread_num() + 1);  // advance rng by 1 ... ncores jumps 
  
#pragma omp for
  for (int i=0; i<n_obs_censusdata; ++i)
    for (int j=0; j<n_bootstrap; j++)
      macc(i,j) = X.row(i) * beta_sample.col(j) + locationeffects[distrloc(lgen)] + residuals[distrres(lgen)];
} 

}





// [[Rcpp::export(.summaryParC)]]
Eigen::MatrixXd summaryParC(Environment fbm,Eigen::VectorXd quantiles, int nrow, int ncol, int ncores) {
  
  // create pointer to File Based Matrix
  XPtr<FBM> xpMat = fbm["address"];
  BMAcc<double> macc(xpMat);
  
  const int no_quantiles = quantiles.size();
  Eigen::MatrixXd result(nrow, no_quantiles + 3);
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
#pragma omp for schedule(dynamic)
  for (int i = 0; i < nrow; i++) {
    Eigen::VectorXd v = macc.row(i);
    for (int q = 0; q < 5; ++q) {
      
      double total = 0;
      double totalsquare = 0;
      
      for (int j = 0; j < ncol; j++){
        total += x(i,j);
        totalsquare += pow(x(i,j),2);
      }
      
      double mean = total / ncol;
      result(i,0) = mean;
      double var = totalsquare / ncol - pow(mean,2);
      result(i,1) = var;
      result(i,2) = sqrt(var);
      
      
      
      std::nth_element(v.data() + indices[q] + 1,
                       v.data() + indices[q+1],
                                         v.data() + v.size());
      result(i,q + 3) = v[indices[q+1]];
    }
  }
}




// 
// 
// 
// 
// // [[Rcpp::export]]
// NumericVector rowmeansBigC(Environment fbm) {
// 
//   XPtr<FBM> xpMat = fbm["address"];
//   BMAcc<double> macc(xpMat);
// 
//   int nrow = macc.nrow();
//   int ncol = macc.ncol();
// 
//   NumericVector out(nrow);
// 
//   for (int i = 0; i < nrow; i++) {
//     double total = 0;
//     for (int j = 0; j < ncol; j++) {
//       total += macc(i, j);
//     }
//     out[i] = total / ncol;
//   }
//   return Rcpp::wrap(out);
// }


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



