context("data imputation")




test_that("imputation of left-right data works", {
  library(dplyr)
  normlr = data.frame(right = rnorm(10000, 100, 1)) %>% 
    mutate(left = ifelse(right < 99, 0, right),
           right = ifelse(right < 99, 99, right))
  
  lnormlr = data.frame(right = rlnorm(1000, 2, 1)) %>% 
    mutate(left = ifelse(log(right) < 1, 0, right),
           right = ifelse(log(right) < 1, exp(1), right))
  
  expect_lt(abs(mean(randlr(normlr, "norm")$imputed) - 100), 0.02)
  expect_lt(abs(sd(randlr(normlr, "norm")$imputed) - 1), 0.02)
  
  expect_lt(abs(median(log(randlr(lnormlr, "norm")$imputed)) - 2), 0.2)
})