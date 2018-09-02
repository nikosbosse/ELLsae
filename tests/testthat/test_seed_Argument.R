library(ELLsae)

context("Testing if the argument seed actualy works for setting seed in C++")

# example data from "clustermeans"
df.survey <- data.frame(y = c(1,2,3,4,1,2,3,4,1),
                        a = c(1,2,3,1,2,3,1,2,3),
                        b = c(5,6,4,8,2,6,9,8,5))

df.census <- data.frame(a = (c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1)),
                        b = c(5,3,7,2,5,4,7,5,1,1,7,9,5,4,7,2),
                        d = c(11,12,13,14,15,16,17,18,19,10,12,13,8,9,7,8))

random_control <- c()
for (i in 1:7) {
ellsae(model = y ~ a + b, clustermeans = "b",
       survey = df.survey, census = df.census,
       location_survey = "a", seed = 12345,
       output = "all", n_boot = 50L)$yboot_est
random_control[i] <- runif(1)
}

test_that("setting a seed actually leads to the same result 
          every time (ellsae)", {
  expect_equal(ellsae(model = y ~ a + b, clustermeans = "b",
                      survey = df.survey, census = df.census,
                      location_survey = "a", seed = 12345,
                      output = "all", n_boot = 50L)$yboot_est, 
               ellsae(model = y ~ a + b, clustermeans = "b",
                      survey = df.survey, census = df.census,
                      location_survey = "a", seed = 12345,
                      output = "all", n_boot = 50L)$yboot_est)
})

test_that("The seed is actually randomized again after the function is done 
          (ellsae)", {
  expect_gte(length(unique(random_control)), 2)
})


test_that("setting a seed actually leads to the same result 
          every time (ellsae_big)", {
            expect_equal(ellsae_big(model = y ~ a + b, clustermeans = "b",
                                survey = df.survey, census = df.census,
                                location_survey = "a", seed = 12345,
                                output = "all", n_boot = 50L)$yboot_est, 
                         ellsae_big(model = y ~ a + b, clustermeans = "b",
                                survey = df.survey, census = df.census,
                                location_survey = "a", seed = 12345,
                                output = "all", n_boot = 50L)$yboot_est)
          })

random_control_big <- c()
for (i in 1:7) {
  ellsae_big(model = y ~ a + b, clustermeans = "b",
         survey = df.survey, census = df.census,
         location_survey = "a", seed = 12345,
         output = "all", n_boot = 50L)$yboot_est
  random_control_big[i] <- runif(1)
}

test_that("The seed is actually randomized again after the function is done 
          (ellsae_big)", {
            expect_gte(length(unique(random_control_big)), 2)
          })
