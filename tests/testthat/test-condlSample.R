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
  data(rc_synth, package = "rcmodel")
  library(dplyr)
  
  mod1 = rcmodel::rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), 
                        rc_synth)
  
  mod2 = rcmodel::rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), 
                        Phosphorus)
  newdata2 = data.frame(Date = as.Date("1986-09-17"), conc = 0.1, 
                       conc.units = "mg/l", flow = 10, 
                       flow.units = "CFS", is.bdl = FALSE)
  
  expect_is(condlSample(mod2, newdata = newdata2, quantile = 0.9),
            "numeric")
  expect_is(condlSample(mod2, quantile = 0.9),
            "numeric")
})

test_that("conditional samples are correct for rcgams", {
  data("rc_synth", package = "rcmodel")
  mod1 = rcmodel::rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), 
                        rc_synth)
  q10 <- condlSample(mod1, quantile = 0.1)
  q90 <- condlSample(mod1, quantile = 0.9)
  
  expect_less_than(mean(q10), mean(rc_synth$conc))
  expect_more_than(mean(q90), mean(rc_synth$conc))
  
  expect_less_than(sum(q10 > rc_synth$conc), 30)
  expect_more_than(sum(q10 > rc_synth$conc), 19)
  expect_less_than(sum(q90 < rc_synth$conc), 30)
  expect_more_than(sum(q90 < rc_synth$conc), 19)
})