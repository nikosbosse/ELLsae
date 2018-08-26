c <- matrix(as.numeric(1:1000), ncol = 10)
b <- matrix(as.numeric(1:10), nrow = 1)
d <- matrix(as.numeric(1:1.5e+08), ncol = 500)

300000*500


mbm <- microbenchmark::microbenchmark(
  summaryC(c, c(0, 0.25, 0.5, 0.75, 1), nrow = 100, ncol = 10, ncores = 4),
  summaryParC(c, c(0, 0.25, 0.5, 0.75, 1), nrow = 100, ncol = 10, ncores = 4)
)

e <- t(d)
a <- Sys.time()
summary(e)
Sys.time() - a


summaryParC(d, c(0, 0.25, 0.5, 0.75, 1))

stats:::quantile

summary(t(b))
summary(t(a))
