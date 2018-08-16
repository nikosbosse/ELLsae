library(ELLsae)

context("Testing of the meanforregression - part of ELLsae")

##################
### Preliminary ###
##################

df.survey <- data.frame(y = c(1,2,3,4,1,2,3,4,1),
                        a = c(1,2,3,1,2,3,1,2,3),
                        b = c(5,6,4,8,2,6,9,8,5))

df.census <- data.frame(a = (c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1)),
                        b = c(5,3,7,2,5,4,7,5,1,1,7,9,5,4,7,2))

test_that("computing the means actually works", {
  expect_equal(ELLsae(model = y ~ a + b, mResponse = "b",
                       surveydata = df.survey, censusdata = df.census,
                       location_survey = "a",
                       test = "meanforregression")$censusmeans$b_meanCensus,
               aggregate(b ~ a, df.census, mean)$b)
})










#lm(model, df.survey)
#
# b.Cmean <- rep(loc.mean[,2], 3)
#
# df.survey.new <- cbind(df.survey, b.Cmean)
# df.survey.new <- df.survey.new[order(a),]
#
# df.survey.new[,4]
# mResponse<- c("b")
#
#
#
# test_that("the function returns nothing if a character vector is put in", {
#   expect_equal(as.data.frame(mean.for.regression(mResponse="b",
#                                                  censusdata=df.census,
#                                                  surveydata=df.survey,
#                                                  model= y ~ a + b,
#                                                  location_survey = "a")[[2]])[,4],df.survey.new[,4])
#   expect_equal(mean.for.regression(mResponse="b",
#                                    censusdata=df.census,
#                                    surveydata=df.survey,
#                                    model= y ~ a + b,
#                                    location_survey = "a")[[1]],
#                "y ~ a + b + b.Cmean")
#   expect_equal(is.list(mean.for.regression(mResponse="b",
#                                            censusdata=df.census,
#                                            surveydata=df.survey,
#                                            model= y ~ a + b,
#                                            location_survey = "a")),
#                TRUE)
#   expect_equal(ncol(as.data.frame(mean.for.regression(mResponse="b",
#                                                       censusdata=df.census,
#                                                       surveydata=df.survey,
#                                                       model= y ~ a + b,
#                                                       location_survey = "a")[[2]])),
#                ncol(df.survey)+length(mResponse))
# })
#
