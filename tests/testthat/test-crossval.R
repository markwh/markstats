context("crossvalidation")

test_that("crossvalidation returns reasonable numbers", {
  data(goodlm)
  
  expect_equal(crossvalidate(goodlm, kfolds = 0, "mse"), 
               crossvalidate(goodlm, kfolds = 0, "rmse")^2)
  
  expect_less_than(crossvalidate(goodlm, kfolds = 0, "R2"), 1)
  expect_less_than(crossvalidate(goodlm, kfolds = 3, "R2"), 1)
})

test_that("lm LOO crossvalidation agrees with press statistic", {
  data(goodlm)
  fakedata = goodlm$model
  goodlm <- eval(goodlm$call)
  expect_equal(crossvalidate(goodlm, statistic = "mse"), qpcR::PRESS(goodlm)$stat / nrow(goodlm$model))
  expect_equal(crossvalidate(goodlm), qpcR::PRESS(goodlm)$P.square)
})