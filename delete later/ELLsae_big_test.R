# # check whether bigstatsr is available
# if(!requireNamespace("bigstatsr", quietly = TRUE)) {
#   stop("Package \"bigstatsr\" needed for this function to work. 
#        Please install it, i.e. run install.packages(bigstatsr)",call. = FALSE)
# }
# 
# 
# 
# 
# 
# 
# 
# 
# ###################################### Der Teil ist anders ######################################
# 
# # bootstrap <- bigstatsr::FBM(nrow = n_obs_census, ncol = n_boot, type = "double")
# bootstrap <- bigstatsr::FBM(nrow = n_boot, ncol = n_obs_census, type = "double")
# 
# .InfCensBigCpp(fbm = bootstrap, 
#                n_bootstrap = n_boot, n_obs_censusdata = n_obs_census,
#                locationeffects = location_effect, 
#                residuals = residuals(model_fit),
#                X = X_census, beta_sample = betas, userseed = seed, ncores = num_cores)
# 
# if(!missing(welfare.function)){
#   bigstatsr::big_apply(bootstrap,
#                        a.FUN = function(bootstrap, ind, fun){
#                          bootstrap[,ind] <- fun(bootstrap[,ind])
#                          NULL
#                        }, 
#                        a.combine = 'c', ncores = 1, 
#                        fun = welfare.function)
# }
# 
# 
# 
# ##################################################################################################
# 
# 
# 
# 
# output_list <- list()
# if(output == "default" | output == "all" | "summary" %in% output){
# 
# 
# 
# ###################################### Der Teil ist anders ######################################
# #tboot <- bigstatsr::big_transpose(bootstrap) 
# # big_apply works more efficiently columnwise
# # summaryboot <- bigstatsr::big_apply(bootstrap,
# #                                     a.FUN = function(bootstrap, ind,
# #                                                      fun, q, boot, c) {
# # 
# #                                       fun(x = bootstrap[,ind],
# #                                           quantiles = q, nrow = boot,
# #                                           ncol = length(ind), ncores = c)
# # 
# #                                     }, a.combine = 'rbind',
# #                                     ncores = 1,
# #                                     fun = .summaryBigParCt, q = quantiles,
# #                                     boot = n_boot, c = num_cores)
# 
# summaryboot <- .summaryBigParCt(fbm = bootstrap, quantiles = quantiles, nrow = n_boot, ncol = n_obs_census, ncores = num_cores)
# ##################################################################################################
# 
# colnames(summaryboot) <- c("mean", "var", "sd", paste(quantiles*100, "%-Quant", sep = ""))
# output_list$summary_boot <- summaryboot
# }
# 
# 
# if(save_boot == T){
#   ##################################################################################################
#   bigstatsr::big_write(bootstrap, paste("BootstrapSampleELLsae-", Sys.Date(),  ".csv", sep = ""), 
#                        every_nrow = 2.5e+07/n_boot) # sensible number of rows to write at a time
#   ##################################################################################################
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # summaryboot <- big_apply(tboot, 
# #                          a.FUN = function(X, ind, q){
# #                            var = apply(X, MARGIN = 2, FUN = var)
# #                            sd = sqrt(var)
# #                            
# #                            return(t(rbind(mean = colMeans(X),
# #                                           var = var,
# #                                           sd = sd,
# #                                           quantile(x, probs = q))))
# #                          } 
# #                          a.combine = 'rbind', q = quantiles)
# 
# 
# # summaryboot <- .summaryBigCt(tboot[], quantiles = quantiles, nrow = n_boot, ncol = n_obs_census)
# 
# # summaryboot <- bigstatsr::big_apply(bootstrap,
# #                      a.FUN = function(bootstrap, ind, fun, n_obs, loc, res, X_census, betas) {
# #                        
# #                        fun(x = bootstrap[ind,], quantiles = q, nrow = length(ind), ncol = boot)
# #                      
# #                        }, a.combine = 'c', ind = bigstatsr::rows_along(bootstrap), 
# #                      ncores = 1, fun = .summaryBigC, boot = n_boot)
# # 
# # #bigstatsr::nb_cores()
# 
