library(ELLsae)

context("the end result is unbiased")

survdata <-  data.frame(y = c(6,2,3,5,4,8), 
                        a = c(1,2,4,5,6,7), 
                        b = c(2,3,4,1,4,1), 
                        c= c(1,3,8,4,6,2))
censdata <- survdata[,-1]
model <- y ~ a + b + c
location_survey <- "c"
mfit <- lm(model, survdata)

y_fit <- predict(mfit, newdata = censdata)

test_that("the result is unbiased without any extra features (ellsea)", {
  expect_equal(round(as.numeric(ellsae(model = model, survey = survdata, 
                            census = censdata, seed = 5,
                            location_survey = "c", 
                            n_boot = 10000)$yboot_est)),
               round(as.numeric(y_fit)))
})

test_that("the result is unbiased without any extra features (ellsea_big)", {
  expect_equal(round(as.numeric(ellsae_big(model = model, survey = survdata, 
                                       census = censdata, seed = 5,
                                       location_survey = "c", 
                                       n_boot = 10000)$yboot_est)),
               round(as.numeric(y_fit)))
})

