# data <- SAE::househoulddatabrazil
# location_effect <- unique(data$geo2_br)
# 
# df.survey <- data.frame(y = c(1,2,3,4,1,2,3,4,1),
#                         a = c(1,2,3,1,2,3,1,2,3),
#                         b = c(5,6,4,8,2,6,9,8,5))
# 
# df.census <- data.frame(a = (c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1)),
#                         b = c(5,3,7,2,5,4,7,5,1,1,7,9,5,4,7,2))
# 
# n_boot <- 50
# model <- y~a+b
# mfit <-lm(model, df.survey)
# 
# beta_sample <- t(MASS::mvrnorm(n = n_boot,
#                                mu = coefficients(mfit),
#                                Sigma = vcov(summary(mfit))))
# 
# t <- terms.formula(model)
# t <- delete.response(t)
# X <- model.matrix(t, df.census)
# 
# 
# # inferenceCensusC(n_bootstrap = 50, n_obs_censusdata = nrow(df.census), 
# #                  locationeffects = as.numeric(5:25),
# #                  residuals = residuals(mfit), X = X, beta_sample = beta_sample)
