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


