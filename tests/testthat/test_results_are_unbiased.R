library(ELLsae)

context("the end result is unbiased")

survdata <-  data.frame(y = c(6,2,3,5,4,8), a = c(1,2,4,5,6,7), b = c(2,3,4,1,4,1), c= c(2,4,3,1,5,7))
censdata <- survdata[,-1]
model <- y ~ a + b + c
location_survey <- "c"
mfit <- lm(model, survdata)

y_pred <- mfit$fitted.values

test_that("the result is unbiased without any extra features", {
  expect_equal(trunc(ELLsae::ellsae(model = model, surveydata = survdata, censusdata = censdata, location_survey = "c", n_boot = 5000)),
                     trunc(y_pred))
})

# Test tha mResponse works

# df.survey <- data.table::data.table(y = c(1,2,3,4,1,2,3,4,1),
#                                     a = c(1,2,3,1,2,3,1,2,3),
#                                     b = c(5,6,4,8,2,6,9,8,5),
#                                     c = c(9,4,5,3,1,2,5,4,8))
# df.census <- data.table::data.table(a = (c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1)),
#                                     b = c(5,3,7,2,5,4,7,5,1,1,7,9,5,4,7,2),
#                                     c = c(5,8,5,4,1,6,8,7,3,5,6,1,2,6,4,8))
# location <- "a"
# df.census.withmean <- df.census 
# df.census.withmean[, c("b_MeanCensus", "c_MeanCensus") := (lapply(.SD, mean)), by = "a"]
# means_from_census <- unique(df.census.withmean[,c("b_MeanCensus", "c_MeanCensus", "a")])
# df.survey.withmean <- merge(df.survey, means_from_census, by = "a", all.x = TRUE)
# model <- y ~ a + b + b_meanCensus + c_meanCensus
# mfit <- lm(model, df.survey.withmean)
# 
# test_that("the result is unbiased using mResponse", {
#   expect_equal(trunc(ELLsae::ELLsae(model = model, surveydata = survdata, censusdata = censdata, location_survey = "c", n_boot = 5000)),
#                trunc(y_pred))
# })
# 
# 
