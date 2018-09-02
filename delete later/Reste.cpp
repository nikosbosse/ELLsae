// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// [[Rcpp::plugins(cpp11)]]



// // [[Rcpp::export(.InfCensBigCpp_alt)]]
// void InfCensBigCpp(Environment fbm, const int n_bootstrap,
//                    const int n_obs_censusdata,
//                    const Eigen::Map<Eigen::VectorXd> locationeffects,
//                    const Eigen::Map<Eigen::VectorXd> residuals,
//                    const Eigen::Map<Eigen::MatrixXd> X,
//                    const Eigen::Map<Eigen::MatrixXd> beta_sample,
//                    int userseed, int ncores)
// {
//   
//   // --------- create random sample of locations and of residuals --------- //
//   
//   // initialise pseudo random number generator and seed it
//   dqrng::xoshiro256plus gen;
//   gen.seed(userseed);
//   
//   // initialize distributions for randam locations and residuals
//   const int upperlocation = locationeffects.size();
//   const int upperresiduals = residuals.size();
//   std::uniform_int_distribution<> distrloc(0, upperlocation - 1);
//   std::uniform_int_distribution<> distrres(0, upperresiduals - 1);
//   
//   // create pointer to File Based Matrix
//   XPtr<FBM> xpMat = fbm["address"];
//   BMAcc<double> macc(xpMat);
//   
//   
//   // initialize and fill matrix for randam locations and residuals
//   Eigen::MatrixXd LocationEffectResiduals(n_obs_censusdata, n_bootstrap);
//   
//   
//   
// #pragma omp parallel num_threads(ncores)
// {
//   dqrng::xoshiro256plus lgen(gen);      // make thread local copy of rng
//   lgen.jump(omp_get_thread_num() + 1);  // advance rng by 1 ... ncores jumps
//   
// #pragma omp for
//   for (int i=0; i<n_obs_censusdata; ++i)
//     for (int j=0; j<n_bootstrap; j++)
//       macc(i,j) = X.row(i) * beta_sample.col(j) + locationeffects[distrloc(lgen)] + residuals[distrres(lgen)];
// }
// 
// }




// 
// // [[Rcpp::export(.summaryParBigC)]]
// SEXP summaryParBigC(Environment fbm,
//                     Eigen::VectorXd quantiles,
//                     IntegerVector col_ind,
//                     int nrow, int ncol, int ncores) {
//   
//   // create pointer to File Based Matrix
//   XPtr<FBM> xpMat = fbm["address"];
//   BMAcc<double> macc(xpMat);
//   
//   // std::vector<int> col_ind(ncol);
//   // std::iota(col_ind.begin(), col_ind.end(), 0);
//   
//   const int no_quantiles = quantiles.size();
//   Eigen::MatrixXd result(nrow, no_quantiles); // + 3);
//   // int indices[no_quantiles +1];
//   // indices[0] = -1;
//   // for (int k=0; k<no_quantiles; k++){
//   //   if (quantiles[k] < 0.5){
//   //     indices[k+1] = floor(quantiles[k] * (ncol-1));
//   //   } else {
//   //     indices[k+1] = ceil(quantiles[k] * (ncol-1));
//   //   }
//   // }
//   
//   
// #pragma omp parallel num_threads(ncores)
// {
// #pragma omp for schedule(dynamic)
//   for (int i = 0; i < nrow; i++) {
//     // SubBMAcc<double> macc(xpMat, i, col_ind);
//     // NumericVector<double> v = macc(); //better to transpose the matrix beforehand.
//     for (int q = 0; q < no_quantiles; ++q) {
//       
//       double total = 0;
//       double totalsquare = 0;
//       
//       for (int j = 0; j < ncol; j++){
//         total += macc(i,j);
//         totalsquare += pow(macc(i,j),2);
//       }
//       
//       double mean = total / ncol;
//       result(i,0) = mean;
//       double var = totalsquare / ncol - pow(mean,2);
//       result(i,1) = var;
//       result(i,2) = sqrt(var);
//       
//       // std::nth_element(v.data() + indices[q] + 1,
//       //                  v.data() + indices[q+1],
//       //                                    v.data() + v.size());
//       // result(i,q + 3) = v[indices[q+1]];
//     }
//   }
// }
// return Rcpp::wrap(result);
// }





// nicht parallelisierte Version 
// // [[Rcpp::export(.summaryBigCt)]]
// SEXP summaryBigCt(Eigen::MatrixXd x,
//                   Eigen::VectorXd quantiles,
//                   int nrow, int ncol) {
//   const int no_quantiles = quantiles.size();
//   Eigen::MatrixXd result(ncol, no_quantiles + 3);
//   int indices[no_quantiles +1];
//   indices[0] = -1;
//   for (int k=0; k<no_quantiles; k++){
//     if (quantiles[k] < 0.5){
//       indices[k+1] = floor(quantiles[k] * (nrow-1));
//     } else {
//       indices[k+1] = ceil(quantiles[k] * (nrow-1));
//     }
//   }
//   
//   {
//     
//     for (int j = 0; j < ncol; j++){
//       Eigen::VectorXd v = x.col(j);
//       for (int q = 0; q < no_quantiles; ++q) {
//         
//         double total = 0;
//         double totalsquare = 0;
//         
//         for (int i = 0; i < nrow; i++){
//           total += x(i,j);
//           totalsquare += pow(x(i,j),2);
//         }
//         
//         double mean = total / nrow;
//         result(j,0) = mean;
//         double var = totalsquare / nrow - pow(mean,2);
//         result(j,1) = var;
//         result(j,2) = sqrt(var);
//         
//         std::nth_element(v.data() + indices[q] + 1,
//                          v.data() + indices[q+1],
//                                            v.data() + v.size());
//         result(j,q + 3) = v[indices[q+1]];
//       }
//     }
//   }

// Eigen::VectorXi indi(no_quantiles +1);
// for (int a=0; a < no_quantiles +1; a++)
//   indi[a] = indices[a];
// return Rcpp::wrap(indi);
// Eigen::VectorXd v = x.col(1);
// return Rcpp::wrap(v);
//return Rcpp::wrap(result);
//}




// 
// // [[Rcpp::export(.rowmeanParC)]]
// SEXP rowmeanParC(Eigen::MatrixXd x, 
//               int nrow, int ncol, int ncores) {
//   Eigen::VectorXd out(nrow);
//   
// #pragma omp parallel num_threads(ncores)
// {
// #pragma omp for schedule(dynamic)
//   for (int i = 0; i < nrow; i++) {
//     double total = 0;
//     for (int j = 0; j < ncol; j++) {
//       total += x(i, j);
//     }
//     out[i] = total / ncol;
//   }
// }
//   
//   return Rcpp::wrap(out);
// }