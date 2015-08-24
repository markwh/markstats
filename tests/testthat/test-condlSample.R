context("conditional samples from model output")


test_that("quantiles are as expected", {
  x = runif(1000, 0, 100)
  y = x / 4 + rnorm(1000, 0, 10)
  datxy = data.frame(x = x, y = y)
  
  lmxy = lm(y ~ x, datxy)
  q10 = condlSample(lmxy, quantile = 0.1)
  q90 = condlSample(lmxy, quantile = 0.9)
  
  expect_less_than(sum(q10 > y), 150)
  expect_more_than(sum(q10 > y), 50)
  
  expect_less_than(sum(q90 < y), 150)
  expect_more_than(sum(q90 < y), 50)
})