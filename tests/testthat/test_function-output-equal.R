library(ELLsae)

context("Are the outputs of ellsea and ellsea_big the same?")

df.survey <- data.frame(y = c(1,2,3,4,1,2,3,4,1),
                        a = c(1,2,3,1,2,3,1,2,3),
                        b = c(5,6,4,8,2,6,9,8,5))
df.census <- data.frame(a = (c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1)),
                        b = c(5,3,7,2,5,4,7,5,1,1,7,9,5,4,7,2),
                        d = c(11,12,13,14,15,16,17,18,19,10,12,13,8,9,7,8))
model = y ~ a + b
location_survey = "a"

test_that("outputs of ellsea and ellsea_big equal?", {
  expect_equal(trunc(ellsae(model = y ~ a + b, clustermeans = "b",
                      surveydata = df.survey, censusdata = df.census,
                      location_survey = "a", seed = 12345,
                      output = "all", n_boot = 50L)$summary_boot[,1]),
               trunc(ellsae_big(model = y ~ a + b, clustermeans = "b",
                                surveydata = df.survey, 
                                censusdata = df.census,
                                location_survey = "a", seed = 12345,
                                output = "all", n_boot = 50L)$summary_boot[,1]))
})
