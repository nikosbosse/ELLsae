library(ELLsae)

# ------------------------------------------------ #
# -------------- function works ------------------ #
# ------------------------------------------------ #

context("Checks if all the objects in the general sae function body interact 
        in the right way")

surveydata <-  data.frame(a = c(1,2,4,5,6,7), 
                          b = c(2,3,4,1,4,1), 
                          c= c(2,4,3,1,5,7))
censusdata <-  data.frame(a = c(1,2,6,7,5,1,39,6), 
                          b = c(2,3,6,3,4,7,2,8), 
                          c= c(2,4,5,1,8,9,6,4))
model <- a ~ b + c
location_survey <- "c"

test_that("the function works", {
  expect_equal(length(ellsae(model, surveydata , censusdata, 
                             location_survey, n_boot = 5, 
                             welfare.function = identity)$yboot_est),
               8)

})

# -------------- welfare functions ------------------ #

test_that("different welfare functions can be introduced", {
  expect_equal(length(ellsae(model, surveydata , censusdata, location_survey,
                             n_boot = 5, seed = 1234, 
                             welfare.function = function(x){2*x})$yboot_est),8)
  expect_equal(length(ellsae(model, surveydata , censusdata, location_survey, 
                             n_boot = 5, 
                             welfare.function = function(x){log(x^2 + 2)})$yboot_est),8)
})




# -------------- inputs ------------------ #

context("Testing whether the function handles inputs 
        the way it is supposed to be")

test_that("the function handles inputs for surveydata correctly", {
  expect_equal(length(ellsae(model, as.matrix(surveydata) , censusdata, 
                             location_survey, n_boot = 5, 
                             welfare.function = identity)$yboot_est),
               8)
  expect_error(length(ellsae(model = model, censusdata = censusdata,
                             location_survey = location_survey, n_boot = 5)),
               "Input surveydata is missing")
})

test_that("the function handles inputs for censusdata correctly", {
  expect_equal(length(ellsae(model, surveydata, 
                             as.matrix(censusdata), location_survey, n_boot = 5, 
                             welfare.function = identity)$yboot_est),
               8)
  expect_error(length(ellsae(model = model, surveydata = surveydata,
                             location_survey = location_survey, n_boot = 5)),
               "Data frame with the censusdata is missing")
})

test_that("the function handles inputs for locations correctly", {
  # input: string = variable name
  expect_equal(length(ellsae(model, surveydata , censusdata, 
                             location_survey = "c",
                             n_boot = 5)$yboot_est),8)
  # input: location vector missing
  expect_error(length(ellsae(model, surveydata , censusdata, n_boot = 5)),
               "you have to provide a string with the variable indicating the
         location in the survey data set")
  # input: string = not a variable name
  expect_error(length(ellsae(model, surveydata , censusdata, location_survey = "d",
                             n_boot = 5)),
               "String that was specified as variable name for the location 
         is not the name of one of the variables in the survey data set.")
})



# Tests für mResponse:
# er meckert, wenn er kein census_location als String bekommt, oder die Variablennamen gleich sind
# er meckert, wenn nicht alle Variablen in mResponse auch in Census enthalten sind
# die Eingabe verschiedener location namen funktioniert
# das mit dem Merging funktioniert, auch wenn in beiden Datensätzen unterschiedliche Variablen sind
# das mit dem Entfernen von Betas aus den Coefficients funktioniert
# die Matrixmultiplikation x'beta funktioniert


# -------------- lm model and prediction with mvnorm ------------------ #

context("lm model prediction and mvnorm sample of x'beta")

predictionELLsae <- function(model, surveydata1 = surveydata, censusdata1 = censusdata){
  model_fit <- lm(model, surveydata1)

  p <- model_fit$rank
  p1 <- seq_len(p)
  piv <- if (p) {stats:::qr.lm(model_fit)$pivot[p1]}
  beta <- model_fit$coefficients
  Terms <- delete.response(terms(model_fit))
  m <- model.frame(Terms, censusdata, na.action = na.pass,
                   xlev = model_fit$xlevels)
  X <- model.matrix(Terms, m, contrasts.arg = model_fit$contrasts)

  predictor <- drop(X[, piv, drop = FALSE] %*% beta[piv])
  return(predictor)
}


surveydata <- data.frame(y = 1:5, 
                         a = c(2,6,3,4,2), 
                         b = c(5,8,5,1,5), 
                         c = c(5,9,3,5,4), 
                         loc = c(1,2,1,2,3))
censusdata <- data.frame(y = 4:9, 
                         a = 14:19, 
                         b = 24:29, 
                         c = 25:30, 
                         loc = c(1,1,3,1,2,2))

model1 <- y ~ b + a; m1 <- lm(model1, surveydata)
model2 <- y ~ b + a; m2 <- lm(model2, surveydata)
model3 <- log(y) ~ b + a; m3 <- lm(model3, surveydata)
model4 <- y ~ b^2 + (a); m4 <- lm(model4, surveydata)
model5 <- y ~ b + log(a); m5 <- lm(model5, surveydata)
model6 <- y ~ b*a; m6 <- lm(model6, surveydata)

test_that("the manual prediction approach works 
          correctly for different models", {
  expect_equal(predictionELLsae(model1), predict.lm(m1, newdata = censusdata))
  expect_equal(predictionELLsae(model2), predict.lm(m2, newdata = censusdata))
  expect_equal(predictionELLsae(model3), predict.lm(m3, newdata = censusdata))
  expect_equal(predictionELLsae(model4), predict.lm(m4, newdata = censusdata))
  expect_equal(predictionELLsae(model5), predict.lm(m5, newdata = censusdata))
  expect_equal(predictionELLsae(model6), predict.lm(m6, newdata = censusdata))

})



predictionELLsae_bootstrap <- function(model, surveydata1 = surveydata, 
                                       censusdata1 = censusdata){
  model_fit <- lm(model, surveydata1)
  
  t <- terms.formula(model)
  t <- delete.response(t)
  X <- model.matrix(t, censusdata)

  beta_sample <- t(MASS::mvrnorm(n = 1000000,
                                 mu = coefficients(model_fit),
                                 Sigma = vcov(summary(model_fit))))

  predictor <- X %*% beta_sample
  return(predictor)
}

# 
# test_that("manually taking a sample of x'beta, e.g. the mean of the 
#           randomly drawn y_pred converges to the true predicted values", {
#   expect_equal(round(rowmeanC(predictionELLsae_bootstrap(model5)),1)
#                ,round(predict.lm(m5, newdata = censusdata),1))
# })
# 

# test_that("ys converge to lm-ys", {
#   expect_equal(round(rowMeans(ellsae(surveydata = )
#     
#     
#   ),1)
#                ,round(predict.lm(m5, newdata = censusdata),1))
# })
# 
# ellsae <- function(model, surveydata, censusdata, location_survey,
#                    n_boot = 50, seed, welfare.function, transfy, transfy_inv, 
#                    output = "default", num_cores = 1, 
#                    quantiles = c(0, 0.25, 0.5, 0.75, 1), clustermeans, 
#                    location_census, save_boot = F)