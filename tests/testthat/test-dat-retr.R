context("retrieving model data")

test_that("getData.lm retrieves model data", {
  set.seed(23)
  fakedat = data.frame(x = rnorm(20), y = rnorm(20))
  orderCols <- function(df) df[order(names(df))]
  expect_identical(orderCols(getData(lm(y ~ x, fakedat))), orderCols(fakedat))
  expect_identical(orderCols(getData(lm(y ~ x, fakedat, model = FALSE))), orderCols(fakedat))
  expect_identical(orderCols(getData(lm(hp ~ disp, mtcars, model = FALSE))), orderCols(mtcars))
})