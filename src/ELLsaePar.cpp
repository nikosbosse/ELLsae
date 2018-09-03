// [[Rcpp::depends(BH, bigstatsr, dqrng, RcppEigen)]]
#include <bigstatsr/BMAcc.h>
#include <RcppEigen.h>
#include <xoshiro.h>
#include <random>

#ifdef _OPENMP
#include <omp.h>
#endif


// [[Rcpp::export(.InfCensCpp)]]
SEXP InfCensCpp(const int n_bootstrap,
                const int n_obs_censusdata, 
                const Eigen::Map<Eigen::VectorXd> locationeffects, 
                const Eigen::Map<Eigen::VectorXd> residuals,
                const Eigen::Map<Eigen::MatrixXd> X, 
                const Eigen::Map<Eigen::MatrixXd> beta_sample, 
                int userseed, int ncores) 
{
  
  // --------- create random sample of locations and of residuals --------- //
  
  // initialise random seeds 
  dqrng::xoshiro256plus gen;
  gen.seed(userseed);
  
  // initialize distributions for randam locations and residuals
  const int upperlocation = locationeffects.size();
  const int upperresiduals = residuals.size();
  
  // subtract 1 because in C++ indices start with 0
  std::uniform_int_distribution<> distrloc(0, upperlocation - 1);
  std::uniform_int_distribution<> distrres(0, upperresiduals - 1);
  
  // initialize and fill matrix for randam locations and residuals 
  Eigen::MatrixXd result(n_obs_censusdata, n_bootstrap);
  
#ifdef _OPENMP
  if (ncores == 999){
    ncores = omp_get_max_threads() - 1;
  }
#endif
  
  
#pragma omp parallel num_threads(ncores)
{
  dqrng::xoshiro256plus lgen(gen);      // make thread local copy of rng 
  
#ifdef _OPENMP
  lgen.jump(omp_get_thread_num() + 1);  // advance rng by 1 ... ncores jumps 
#endif
  
  
#pragma omp for 
  for (int i=0; i<n_obs_censusdata; ++i)
    for (int j=0; j<n_bootstrap; j++)
      result(i,j) = X.row(i)* beta_sample.col(j) + locationeffects[distrloc(lgen)] + residuals[distrres(lgen)];
} 

return Rcpp::wrap(result);

}


// [[Rcpp::export(.summaryParC)]]
SEXP summaryParC(Eigen::Map<Eigen::MatrixXd> x,
                 Eigen::VectorXd quantiles, 
                 int nrow, int ncol, int ncores) {
  
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
  
#ifdef _OPENMP
  if (ncores == 999){
    ncores = omp_get_max_threads() - 1;
  }
#endif
  
#pragma omp parallel num_threads(ncores)
{
#pragma omp for schedule(dynamic)
  for (int i = 0; i < nrow; i++) {
    Eigen::VectorXd v = x.row(i);
    for (int q = 0; q < no_quantiles; ++q) {
      
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
return Rcpp::wrap(result);
}


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
  
#ifdef _OPENMP
  if (ncores == 999){
    ncores = omp_get_max_threads() - 1;
  }
#endif 
  
#pragma omp parallel num_threads(ncores)
{
  dqrng::xoshiro256plus lgen(gen);      // make thread local copy of rng
  lgen.jump(omp_get_thread_num() + 1);  // advance rng by 1 ... ncores jumps
  
#pragma omp for 
  for (int i=0; i<n_obs_censusdata; ++i)
    for (int j=0; j<n_bootstrap; j++)
      macc(j,i) = X.row(i) * beta_sample.col(j) + locationeffects[distrloc(lgen)] + residuals[distrres(lgen)];
}

}





// [[Rcpp::export(.summaryBigParCt)]]
SEXP summaryBigParCt(Environment fbm,
                  Eigen::VectorXd quantiles,
                  int nrow, int ncol, int ncores){
  
  XPtr<FBM> xpMat = fbm["address"];
  BMAcc<double> macc(xpMat);
  
  const int no_quantiles = quantiles.size();
  Eigen::MatrixXd result(ncol, no_quantiles + 3);
  
  
  int indices[no_quantiles +1];
  indices[0] = -1;
  for (int k=0; k<no_quantiles; k++){
    if (quantiles[k] < 0.5){
      indices[k+1] = floor(quantiles[k] * (nrow-1));
    } else {
      indices[k+1] = ceil(quantiles[k] * (nrow-1));
    }
  }
  
#ifdef _OPENMP
  if (ncores == 999){
    ncores = omp_get_max_threads() - 1;
  }
#endif
  
  #pragma omp parallel num_threads(ncores)
  {
  #pragma omp for schedule(dynamic)
    for (int j = 0; j < ncol; j++){
      for (int q = 0; q < no_quantiles; ++q) {
        
        double total = 0;
        double totalsquare = 0;
        
        for (int i = 0; i < nrow; i++){
          total += macc(i,j);
          totalsquare += pow(macc(i,j),2);
        }
        
        double mean = total / nrow;
        result(j,0) = mean;
        double var = totalsquare / nrow - pow(mean,2);
        result(j,1) = var;
        result(j,2) = sqrt(var);
        
        std::nth_element(&macc(0,j) + indices[q] + 1,
                         &macc(0,j) + indices[q+1],
                         &macc(0,j+1));
        result(j,q + 3) = macc(indices[q+1],j);
      }
    }
  }
  return Rcpp::wrap(result);
}






