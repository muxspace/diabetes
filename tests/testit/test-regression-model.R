library(futile.logger)

assert('split_glucose produces correct split ratio', {
  n <- 1000
  df <- data.frame(a=rnorm(n), b=rnorm(n))
  pair <- split_glucose(df, .2)
  flog.info("Count", sapply(pair,nrow), capture=TRUE)
  (nrow(pair$train) - 800 < 50)
  (nrow(pair$test) - 200 < 50)
})

assert('split_glucose creates same dataset with fixed seed', {
  n <- 1000
  df <- data.frame(a=rnorm(n), b=rnorm(n))
  pair.1 <- split_glucose(df, .2, seed=123)
  pair.2 <- split_glucose(df, .2, seed=123)
  (identical(pair.1, pair.2))
})
