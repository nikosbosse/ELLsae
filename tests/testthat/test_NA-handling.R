library(ELLsae)

context("Testing the NA handling of ellsea function")

# example data from "clustermeans"
df.survey <- data.frame(y = c(1,2,3,4,1,2,3,4,1),
                        a = c(1,2,3,1,2,3,1,2,3),
                        b = c(5,6,4,8,2,6,9,8,5))

df.survey.NAexpl <- data.frame(y = c(1,2,3,4,1,2,3,4,1),
                        a = c(1,2,3,1,2,3,1,2,3),
                        b = c(5,6,4,NA,2,6,9,8,5))
#df.survey.NAexpl <- as.data.table(df.survey.NAexpl)

nrow.NAexpl <-nrow(df.survey.NAexpl) - 1

df.survey.NAloc <- data.frame(y = c(1,2,3,4,1,2,3,4,1),
                              a = c(1,2,3,NA,2,3,1,2,3),
                              b = c(5,6,4,8,2,6,9,8,5))

df.census <- data.frame(a = (c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1)),
                        b = c(5,3,7,2,5,4,7,5,1,1,7,9,5,4,7,2),
                        d = c(11,12,13,14,15,16,17,18,19,10,12,13,8,9,7,8))
#df.census <- data.tabe::as.data.table(df.census)

df.census.NAloc <- data.frame(a = (c(1,2,3,1,2,3,1,NA,3,1,2,3,1,2,3,1)),
                        b = c(5,3,7,2,5,4,7,5,1,1,7,9,5,4,7,2),
                        d = c(11,12,13,14,15,16,17,18,19,10,12,13,8,9,7,8))
model = y ~ a + b
response <- all.vars(model)[1]
explanatories <- all.vars(model)[-1]
location_survey = "a"


test_that("If there are NA in the explanatories they are correctly omitted", {
  expect_warning(nrow(ellsae(model = y ~ a + b, clustermeans = "b",
                           surveydata = df.survey.NAexpl, censusdata = df.census,
                           location_survey = "a", seed = 12345,
                           output = "all", n_boot = 50L)$survey), 
                 "your surveydata had missing values. Affected rows were removed.")
})

test_that("If there are NA in the location_census variable and clustermeans
          is used, then an error is thrown", {
  expect_error(ellsae(model = y ~ b, clustermeans = "b",
                      surveydata = df.survey, 
                      censusdata = df.census.NAloc,
                      location_survey = "a", seed = 12345,
                      output = "all", n_boot = 50L))
  
            
})
