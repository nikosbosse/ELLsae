library(ELLsae)

context("Tests whether the function handles the custom quantlies for the
        output the way they are intended")

surveydata <-  data.frame(a = c(1,2,4,5,6,7), 
                          b = c(2,3,4,1,4,1), 
                          c= c(2,4,3,1,5,7))
censusdata <-  data.frame(a = c(1,2,6,7,5,1,39,6), 
                          b = c(2,3,6,3,4,7,2,8), 
                          c= c(2,4,5,1,8,9,6,4))
model <- a ~ b + c
location_survey <- "c"

test_that("Only meaningful quantiles are computed", {
  
  expect_equal(length(ellsae(model, surveydata , censusdata, 
                             location_survey, n_boot = 5, 
                             welfare.function = identity,
                             quantiles = c(5, 0.5, -10, -0.7, 0.2, 1)
                             )$summary_boot), 6)
  
  expect_warning(ellsae(model, surveydata , censusdata, 
                        location_survey, n_boot = 5, 
                        welfare.function = identity,
                        quantiles = c(5, 0.5, -10, -0.7, 0.2, 1)))
})

ellsae(model, surveydata , censusdata, 
       location_survey, n_boot = 5, 
       welfare.function = identity)
