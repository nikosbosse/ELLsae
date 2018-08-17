

data <- data.frame(y = c(1,25,6,4,93,8,21,2,6,4,7,9),
                   a = c(1,95,8,2,52,7,29,8,7,6,1,5),
                   b = c(1,82,4,9,42,8,16,2,5,7,8,2))

model <- y~a+b
mfit <- lm(y~a+b, data)


mfit$coefficients


beta_sample <- t(MASS::mvrnorm(n = 200,
                               mu = coefficients(mfit),
                               Sigma = vcov(summary(mfit))))

t <- terms.formula(model)
t <- delete.response(t)
X <- model.matrix(t, data)

ys_pred <- X %*% beta_sample
y_pred <- mfit$fitted.values
y_avg <- rowmeanC(ys_pred)
