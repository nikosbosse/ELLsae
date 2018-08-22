# data <- SAE::househoulddatabrazil
# location_effect <- unique(data$geo2_br)
# 
# 
# # option 6: bigstatsr
# n_obs_census <- 300000
# n_boot = 250
# maxsize <- 300000000 # 300000000 #arbitrarily define a upper limit per submatrix, here: 300mb
# numentries_max <- maxsize / 8 # assume that one entry in a matrix / data.frame takes up 8 bytes
# num_boot_max <- floor(numentries_max / n_obs_census)
# if(n_boot > num_boot_max){
#   rest <- n_boot %% num_boot_max
#   if(rest == 0){rest <- num_boot_max}
#   number_of_chunks <- ceiling(n_boot / num_boot_max)
#   vector_of_num_boots <- c(rep(num_boot_max, number_of_chunks - 1), rest)
# } else {
#   number_of_chunks <- 1
#   vector_of_num_boots <- n_boot
# }
# 
# random_location_boot <- bigstatsr::FBM(n_obs_census, n_boot, type = "double")
# ind <- c(0, cumsum(vector_of_num_boots))
# 
# a <- Sys.time()
# 
# tmp <- foreach(i=1:number_of_chunks, 
#                .packages=c("ELLsae", "Rcpp"), 
#                .combine = "c") %do% {
#                  
#                  random_location_boot[,(ind[i] + 1):ind[i +1] ] <- ELLsae::rddrawmatrixC(num_unique_elements = length(location_effect),
#                                                                                          n_bootstrap = vector_of_num_boots[i], n_obs_censusdata = n_obs_census,
#                                                                                          elements_to_draw_from = location_effect)
#                  NULL
#                  
#                }
# b <- Sys.time() - a;b
# 
# 
# 
# 
# # option 1: writing to file
# filenames <- foreach(i = 1:number_of_chunks) %do% {tempfile("random_location", fileext = ".csv")}
# a <- sys.Time()
# foreach(i=iterators::iter(1:number_of_chunks), .packages=c("ELLsae", "Rcpp")) %do% {
# 
#   data.table::fwrite(data.table::as.data.table(ELLsae::rddrawmatrixC(num_unique_elements = length(location_effect),
#                                   n_bootstrap = n_boot, n_obs_censusdata = vector_of_num_rows[i],
#                                   elements_to_draw_from = location_effect)),
#             file = filenames[[i]], append = T)
# }
# b <- Sys.Time() - a
# 
# # option 2: iterator
# a <- Sys.time()
# random_location_boot <- foreach(i=iterators::iter(1:number_of_chunks), .packages=c("ELLsae", "Rcpp")) %do% {
# 
#   (ELLsae::rddrawmatrixC(num_unique_elements = length(location_effect),
#                                                                      n_bootstrap = n_boot, n_obs_censusdata = vector_of_num_rows[i],
#                                                                      elements_to_draw_from = location_effect))
# 
# }
# b <- Sys.time() - a
# 
# 
# # option 3: for loop
# random_location_boot <- vector("list", number_of_chunks)
# a <- Sys.time()
# for(i in 1:number_of_chunks){
# 
#   random_location_boot[[i]] <- matrix(0, nrow = vector_of_num_rows[i], ncol = n_boot)
#   # random_location_boot[[i]] <- (ELLsae::rddrawmatrixC(num_unique_elements = length(location_effect),
#   #                                                     n_bootstrap = n_boot, n_obs_censusdata = vector_of_num_rows[i],
#   #                                                     elements_to_draw_from = location_effect))
# }
# b <- Sys.time() - a
# 
# 
# 
# # option 4: parallel computing
# 
# no_cores <- parallel::detectCores() - 3
# cl <- parallel::makeCluster(no_cores)
# doParallel::registerDoParallel(cl)
# 
# random_location_boot2 <- vector("list", number_of_chunks)
# c <- Sys.time()
# random_location_boot2 <- foreach(i=iterators::iter(1:number_of_chunks), .packages=c("ELLsae", "Rcpp")) %dopar% {
# 
#   (ELLsae::rddrawmatrixC(num_unique_elements = length(location_effect),
#                          n_bootstrap = 5000, n_obs_censusdata = vector_of_num_rows[i],
#                          elements_to_draw_from = location_effect))
# }
# parallel::stopCluster(cl)
# d <- Sys.time() - c
# 
# 
# # option 5 GPU computing
# 
# # option 6 data.table
# n_obs_census <- 300000
# random_location_boot <- data.table(V1 = numeric(n_obs_census))
# n_boot = 5000
# maxsize <- 300000000 # 300000000 #arbitrarily define a upper limit per submatrix, here: 300mb
# numentries_max <- maxsize / 8 # assume that one entry in a matrix / data.frame takes up 8 bytes
# num_boot_max <- floor(numentries_max / n_obs_census)
# if(n_boot > num_boot_max){
#   rest <- n_boot %% num_boot_max
#   if(rest == 0){rest <- num_boot_max}
#   number_of_chunks <- n_boot %/% num_boot_max + 1
#   vector_of_num_boots <- c(rep(num_boot_max, number_of_chunks - 1), rest)
# } else {
#   number_of_chunks <- 1
#   vector_of_num_boots <- n_boot
# }
# a <- Sys.time()
# for(i in 1:41){
#   set(x = random_location_boot, 
#       j = as.character(1:vector_of_num_boots[i] + num_boot_max*(i-1)), 
#       value = as.data.table(ELLsae::rddrawmatrixC2(n_bootstrap = vector_of_num_boots[i], 
#                                                    n_obs_censusdata = n_obs_census,
#                                                    elements_to_draw_from = location_effect)))
# }
# 
# 
# 
# 
# 
# 
# 
# # parallel::stopCluster(cl)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# fread(input = filenames[[1]])
# 
# foreach(i=iter(1:number_of_chunks), .packages=c("ELLsae", "Rcpp")) %do% {
# 
#   write.csv(ELLsae::rddrawmatrixC(num_unique_elements = length(location_effect),
#                                   n_bootstrap = n_boot, n_obs_censusdata = vector_of_num_rows[i],
#                                   elements_to_draw_from = location_effect),
#             file = filenames[[i]], append = T)
# }
# #
# 
# 
# 
# 
# 
# 
# 
# 
# # random_location_boot <- as.data.table(matrix(nrow = n_obs_census, ncol = n_boot))
# # foreach(j = iter(random_location_boot, by = "row", chunksize =num_row_max), i=iter(1:number_of_chunks), .packages=c("ELLsae", "Rcpp")) %do% {
# #   j <- as.data.table(ELLsae::rddrawmatrixC(num_unique_elements = length(location_effect),
# #                              n_bootstrap = n_boot, n_obs_censusdata = vector_of_num_rows[i],
# #                              elements_to_draw_from = location_effect))
# # }
# 
# 
# 
# 
# 
# 
