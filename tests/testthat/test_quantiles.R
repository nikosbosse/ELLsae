library(ELLsae)

context("Tests whether the function handles the custom quantlies for the
        output the way they are intended")

survey <-  data.frame(a = c(1,2,4,5,6,7), 
                          b = c(2,3,4,1,4,1), 
                          c= c(2,4,3,1,5,7))
census <-  data.frame(a = c(1,2,6,7,5,1,39,6), 
                          b = c(2,3,6,3,4,7,2,8), 
                          c= c(2,4,5,1,8,9,6,4))
model <- a ~ b + c
location_survey <- "c"

test_that("Only meaningful quantiles are computed (ellsea)", {
  # because the model also throws a warning
  expect_equal(suppressWarnings(
    ncol(ellsae(model, survey , 
                census, 
                location_survey, n_boot = 5, 
                welfare.function = identity,
                quantiles = c(5, 0.5, -10, -0.7, 0.2, 1)
                )$summary_boot)), 6)
  # test the warning is correct
  expect_warning(ncol(ellsae(model, survey , census, 
                           location_survey, n_boot = 5, 
                           welfare.function = identity,
                           quantiles = c(5, 0.5, -10, -0.7, 0.2, 1)
  )$summary_boot), "quantiles < 0 and >1 are automatically omitted")
  
  expect_warning(ellsae(model, survey , census, 
                        location_survey, n_boot = 5, 
                        welfare.function = identity,
                        quantiles = c(5, 0.5, -10, -0.7, 0.2, 1)))
})

test_that("Only meaningful quantiles are computed (ellsea_big)", {
  # because the model also throws a warning
  expect_equal(suppressWarnings(
    ncol(ellsae_big(model, survey , 
                census, 
                location_survey, n_boot = 5, 
                welfare.function = identity,
                quantiles = c(5, 0.5, -10, -0.7, 0.2, 1)
    )$summary_boot)), 6)
  # test the warning is correct
  expect_warning(ncol(ellsae_big(model, survey , census, 
                             location_survey, n_boot = 5, 
                             welfare.function = identity,
                             quantiles = c(5, 0.5, -10, -0.7, 0.2, 1)
  )$summary_boot), "quantiles < 0 and >1 are automatically omitted")
  
  expect_warning(ellsae_big(model, survey , census, 
                        location_survey, n_boot = 5, 
                        welfare.function = identity,
                        quantiles = c(5, 0.5, -10, -0.7, 0.2, 1)))
})
