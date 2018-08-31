library(data.table)
data <- SAE::househoulddatabrazil
helper <- sample(x = 1:nrow(data), size = nrow(data)/5, replace = F)
helper <- sort(helper)
surv <- data[helper,]
cens <- data[-helper,]
rm(list = "helper") #delete helper

mod <-  hh_inc ~ age + urban + rooms + sex + religion + race + adults + children
loc <- "geo2_br"

library(ELLsae)
library(profvis)

#ohne Parallelisierung, ohne FBM
(prof <- profvis({
  #y <- ellsae(model = mod, surveydata = surv, quantiles = c(0, 0.4,0.8, 1), censusdata = cens, location_survey = loc, n_boot = 250L, seed = 5, num_cores = 7)
  y <- ELLsae_big(model = mod, surveydata = surv, quantiles = c(0, 0.4,0.8, 1), censusdata = cens, location_survey = loc, n_boot = 2000, seed = 5, num_cores = 6)
}))


all(round(y$summary_boot[1:5,1:8], 6) == round(y2$summary_boot[1:5,1:8], 6))


# some numeric randomness?
all(round(y$bootsample[1,], 5) == round(y2$bootsample[], 5))


y$summary_boot[1:5,1:8]
y2$summary_boot[1:5,1:8]
min(y$bootsample[5,])

b <- y$bootsample[]
tb <- t(b)

.summaryBigCt(tb, quantiles = c(0,0.25,0.5,0.75,1), nrow = nrow(tb), ncol = ncol(tb))
.summaryParC(b, quantiles = c(0,0.25,0.5,0.75,1), nrow = nrow(b), ncol = ncol(b), ncores = 6)




#ohne Parallelisierung, ohne FBM
prof2 <- profvis({
  y <- ELLsae_base(model = mod, surveydata = surv, censusdata = cens, location_survey = loc, n_boot = 250, num_cores = 8)
}); prof2


#mit Parallelisierung, ohne FBM
prof2 <- profvis({
  y <- ELLsae::ELLsae(model = mod, surveydata = surv, censusdata = cens, location_survey = loc, n_boot = 250, parallel = T)
})

#ohne Parallelisierung mit FBM
prof3 <- profvis({
  y <- ELLsae::ELLsaeBig(model = mod, surveydata = surv, censusdata = cens, location_survey = loc, n_boot = 250, parallel = F)
})

debugonce(ELLsae2)

prof3 <- profvis({
  y <- ELLsae::ELLsaeBig(model = mod, surveydata = surv, censusdata = cens, location_survey = loc, n_boot = 50, parallel = T)
})

# debugonce(ELLsae::ELLsaeBig)














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
