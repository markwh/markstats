context("ggplot functions")

test_that("ggTermPlot works for gam objects", {
  data(goodgam)
  expect_is(ggTermPlot(goodgam), "gg")
})