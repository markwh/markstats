context("ggplot functions")

test_that("ggTermPlot works for gam objects", {
  data(goodgam)
  gg1 <- ggTermPlot(goodgam)
  expect_is(gg1, "gg")
})