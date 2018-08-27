# library(doParallel)
# library(foreach)
# 
# no_cores <- detectCores() - 1
# cl <- makeCluster(no_cores)
# registerDoParallel(cl)
# stopCluster(cl)
# 
# 
# no_cores <- 7
# numuni <- 5
# n_bootstrap = 5
# n_obs_census = 4
# elements <- 1:5
# n_boot <- 10
# 
# maxsize <- 300000000 #arbitrarily define a upper limit, here: 300mb
# numentries_max <- maxsize / 8 # assume that one entry in a matrix / data.frame takes up 8 bytes
# num_row_max <- floor(numentries_max / n_boot)
# num_row_result <- n_obs_census
# 
# if(num_row_result > num_row_max){
#   rest <- num_row_result %% num_row_max
#   if(rest == 0){rest <- num_row_max}
#   number_of_chunks <- num_row_result %/% num_row_max + 1
#   vector_of_num_rows <- c(rep(num_row_max, number_of_chunks - 1), rest)
# } else {
#   number_of_chunks <- 1
#   vector_of_num_rows <- n_obs_census
# }
# 
# random_location_boot <- vector("list", 1)
# random_location_boot <- foreach(j = iter(random_location_boot), i=1:number_of_chunks, .packages=c("ELLsae", "Rcpp")) %do% {
#   j <- ELLsae::rddrawmatrixC(num_unique_elements = numuni,
#                              n_bootstrap = n_boot, n_obs_censusdata = vector_of_num_rows[i],
#                              elements_to_draw_from = elements)
# }
# 
# 
# bootstrap_res <- foreach(i=1:number_of_chunks, .packages=c("ELLsae", "Rcpp")) %do% ELLsae::rddrawmatrixC(num_unique_elements = numuni,
#                                                                                                                 n_bootstrap = n_boot, n_obs_censusdata = vector_of_num_rows[i],
#                                                                                                                 elements_to_draw_from = elements)
# 
# 
# # data taken vom themvnormsampl
# 
# ?split.data.frame
# 
# number_of_chunks <- 3
# vector_of_num_rows <- c(4,4,4)
# 
# splitvector <- rep(as.factor(seq.int(from = 1, to = number_of_chunks)), times = vector_of_num_rows)
# 
# X2 <- split(X, splitvector)
# class(X[[1]])
# 
# Xbeta <- foreach(i = 1:number_of_chunks) %do% {X[[i]] %*% beta_sample}
# 
# 
# if (export){
#   if(missing(filename)){
#     filename <- file.choose()
#   }
#   foreach(i=1:number_of_chunks) %do% {
#     write.csv(listres[[i]], file = filename, append = T)
#   }
# } else {
#   foreach(i=1:number_of_chunks) %do% {
#     listres[[i]] <- as.data.table(listres[[i]])
#   }
#    result <- rbindlist(listres)
# }
# 
# 
# 
# 
# 
# 
# 
# # Memory calculations
# #
# # # every entry takes approximately 8 Bytes
# # max_size_per_obj = 500000000 #maximum size: 500mb
# #
# # approx_size_return_obj <- n_obs_census * n_boot * 8
# # object.size(runif(62500000))
# #
# # n_obs_census * 500 * 8
# #
# # sizec <- object.size(c)
# # dim(c)
# # a <- dim(c)[1] * dim(c)[2]
# # sizea <- object.size(runif(3086116))
# # sizec
# # sizea
