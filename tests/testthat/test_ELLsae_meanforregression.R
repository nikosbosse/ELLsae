library(ELLsae)

context("Testing of the clustermean - part of ELLsae")

###################
### Preliminary ###
###################

df.survey <- data.frame(y = c(1,2,3,4,1,2,3,4,1),
                        a = c(1,2,3,1,2,3,1,2,3),
                        b = c(5,6,4,8,2,6,9,8,5))

df.census <- data.frame(a = (c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1)),
                        b = c(5,3,7,2,5,4,7,5,1,1,7,9,5,4,7,2),
                        d = c(11,12,13,14,15,16,17,18,19,10,12,13,8,9,7,8))

test_that("computing and attaching the means actually works", {
  expect_equal(ellsae(model = y ~ a + b, clustermeans = "b",
                       survey = df.survey, census = df.census,
                       location_survey = "a",
                       output = "all", n_boot = 50L)$survey$b_meanCensus,
               rep(aggregate(b ~ a, df.census, mean)$b, each = 3))
})

test_that("All the error messages are pasted correctly at the right time", {
  #we compute means for a variable that isn't even part of the model
  expect_warning(ellsae(model = y ~ a + b, clustermeans = "d",
                      survey = df.survey, census = df.census,
                      location_survey = "a",
                      output = "all", n_boot = 50L))
  expect_error(ellsae(model = y ~ a + b, clustermeans = "b",
                      survey = df.survey, census = df.census,
                      location_survey = "a", location_census = "d",
                      output = "all", n_boot = 50L))
})
