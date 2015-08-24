context("data imputation")




test_that("imputation of left-right data works", {
  library(dplyr)
  normlr = data.frame(right = rnorm(10000, 100, 1)) %>% 
    mutate(left = ifelse(right < 99, 0, right),
           right = ifelse(right < 99, 99, right))
  
  lnormlr = data.frame(right = rlnorm(1000, 2, 1)) %>% 
    mutate(left = ifelse(log(right) < 1, 0, right),
           right = ifelse(log(right) < 1, exp(1), right))
  
  expect_less_than(abs(mean(randlr(normlr, "norm")$imputed) - 100), 0.01)
  expect_less_than(abs(sd(randlr(normlr, "norm")$imputed) - 1), 0.01)
  
  expect_less_than(abs(median(log(randlr(lnormlr, "norm")$imputed)) - 2), 0.1)
})