context("crossvalidation")

test_that("crossvalidation returns reasonable numbers", {
  data(goodlm)
  
  expect_equal(crossvalidate(goodlm, kfolds = 0, "mse"), 
               crossvalidate(goodlm, kfolds = 0, "rmse")^2)
  
  expect_less_than(crossvalidate(goodlm, kfolds = 0, "R2"), 1)
  expect_less_than(crossvalidate(goodlm, kfolds = 3, "R2"), 1)
})

test_that("lm LOO crossvalidation agrees with press statistic", {
  data(mtcars)
  
  lm1 <- lm(mpg ~ disp, mtcars)
  
  expect_equal(crossvalidate(lm1, statistic = "mse"), qpcR::PRESS(lm1)$stat / nrow(lm1$model))
  expect_equal(crossvalidate(lm1), qpcR::PRESS(lm1)$P.square)
})


## crossvalidate.rcgam

test_that("crossvalidation works for rcgams", {
  
  data(Phosphorus, package = "rcmodel")
  foo = rcmodel::makeModelData(Phosphorus)
  mod2 = rcmodel::rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), foo)
  
  conccvs = c(
    concloo = crossvalidate(mod2, what = "conc"),
    conc5f = crossvalidate(mod2, what = "conc", kf = 5),
    concloo_nosmear = crossvalidate(mod2, what = "conc", smear = FALSE),
    conc5f_nosmear = crossvalidate(mod2, what = "conc", smear = FALSE, kf = 5)
  )
  
  loadcvs = c(
    loadloo = crossvalidate(mod2, what = "load"),
    load5f = crossvalidate(mod2, what = "load", kf = 5),
    loadloo_nosmear = crossvalidate(mod2, what = "load", smear = FALSE),
    load5f_nosmear = crossvalidate(mod2, what = "load", smear = FALSE, kf = 5)
  )
  
  expect_is(conccvs, "numeric")
  expect_is(crossvalidate(mod2, kfolds = 5), "numeric")
  
  expect_true(all(conccvs < 1))
  expect_true(all(loadcvs < 1))
  
})

test_that("crossvalidation works for log-space estimates in rcgams", {
  data(Phosphorus, package = "rcmodel")
  foo = rcmodel::makeModelData(Phosphorus)
  mod2 = rcmodel::rcgam(c ~ s(q) + s(doy, bs = "cc", k = 4) + s(time), foo)
  
  concloo = crossvalidate(mod2, what = "conc", retransform = FALSE, smear = FALSE)
  
  conc5f = crossvalidate(mod2, what = "conc", retransform = FALSE, kf = 5)
  concloo_nosmear = crossvalidate(mod2, what = "conc", retransform = FALSE,
                                  smear = FALSE)
  
  expect_is(concloo, "numeric")
  expect_is(conc5f, "numeric")
  
  expect_less_than(concloo, 1)
  expect_less_than(conc5f, 1)
  expect_equal(concloo, concloo_nosmear)
  
  expect_error(crossvalidate(mod2, what = "load", retransform = FALSE))
  
  #   mod2$gcv.ubre
  #   crossvalidate(mod2, retransform = FALSE, statistic = "mse")
})