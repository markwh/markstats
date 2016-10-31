context("goodness-of-fit")


test_that("coefficient of determination works", {
  
  xy <- data.frame(x = 1:10, y = rnorm(10) + 1:10)
  yhat <- lm(y~x, xy)$fitted
  
  expect_equal(R2(xy$y, yhat), with(xy, cor(x, y)^2))
  expect_equal(R2(xy$y, yhat), 1 - sum((xy$y - yhat)^2) / sum((xy$y - mean(xy$y))^2))
})

test_that("my gof functions agree with built-in", {
  data(goodlm)
  expect_equal(rstudent.gam(goodlm), rstudent(goodlm))
  
  expect_equal(dffits.gam(goodlm), dffits(goodlm))
})