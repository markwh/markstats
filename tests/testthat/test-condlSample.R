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

# test_that("condlSample works with type = 'terms'", {
#   expect_is()
# })


test_that("condlSample works for rcgam objects", {
  data(Phosphorus, package = "rcmodel")
  library(dplyr)
  mod2 = rcmodel::rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), 
                        Phosphorus)
  newdata = data.frame(Date = as.Date("1986-09-17"), conc = 0.1, 
                       conc.units = "mg/l", flow = 10, 
                       flow.units = "CFS", is.bdl = FALSE)
  expect_is(condlSample(mod2, newdata = newdata, quantile = 0.9),
            "numeric")
})