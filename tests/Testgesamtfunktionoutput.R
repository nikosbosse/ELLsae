

mod <-  hh_inc ~ age + urban + rooms + sex + religion + race + adults + children
loc <- "geo2_br"

for(i in 100:101){
  set.seed(i)
  helper <- sample(x = 1:nrow(data), size = nrow(data)/5, replace = F)
  helper <- sort(helper)
  surv <- data[helper,]
  cens <- data[-helper,]
  y <- ELLsae2(model = mod, surveydata = surv, censusdata = cens, location_survey = loc, n_boot = 200, test = "yboot")
  fit <- mean(rowmeanC(y$ymvnorm + y$locboot + y$resboot))
  cat(i, ":", "\n",
      "meanymvnrom", mean(rowmeanC(y$ymvnorm)), "\n",
      "meanlocboot", mean(rowmeanC(y$locboot)), "\n",
      "meanresboot", mean(rowmeanC(y$resboot)), "\n",
      "difference y_pred - y_true", fit - truemean, "\n", "\n", sep = " ")
}
y_true <- cens[,names(cens) == "hh_inc"]
truemean <- mean(y_true)


library(profvis)

prof <- profvis({
  ELLsae2(model = mod, surveydata = surv, censusdata = cens, location_survey = loc, n_boot = 400)
})

nrow(cens) * 400 * 8



# Memory calculations
#
# # every entry takes approximately 8 Bytes
# max_size_per_obj = 500000000 #maximum size: 500mb
#
# approx_size_return_obj <- n_obs_census * n_boot * 8
# object.size(runif(62500000))
#
# n_obs_census * 500 * 8
#
# sizec <- object.size(c)
# dim(c)
# a <- dim(c)[1] * dim(c)[2]
# sizea <- object.size(runif(3086116))
# sizec
# sizea

#
#
#
#
# helper <- sample(x = 1:nrow(data), size = nrow(data)/5, replace = F)
# helper <- sort(helper)
# surv <- data[helper,]
# cens <- data[-helper,]
# rm(list = "helper") #delete helper
#
#
# y <- ELLsae2(model = mod, surveydata = surv, censusdata = cens, location_survey = loc, n_boot = 50, test = "yboot")
# y_true <- cens[,names(cens) == "hh_inc"]
#
# # compare means
# y_pred <- rowmeanC(y$ymvnorm + y$locboot + y$resboot)
# mean(y_pred)
# truemean <- mean(y_true)
# mean(y$ysimplpred)
#
# mean(rowmeanC(y$ymvnorm))
# mean(rowmeanC(y$locboot))
# mean(rowmeanC(y$resboot))
#
# SAE::rddrawmatrixC()
#
# hist(y_true)
# ?hist
#
#
