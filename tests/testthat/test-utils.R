context("utils functions")

test_that("bind_rows2 converts disperate-class columns", {
  df1 <- df2 <- mtcars
  df2$cyl <- as.factor(df2$cyl)
  
  expect_silent(df3 <- bind_rows2(list(df1, df2)))
  expect_message(df3 <- bind_rows2(list(df1, df2), verbose = TRUE))
  expect_equal(nrow(df1), nrow(df2))
  expect_equal(nrow(df3), 2 * nrow(df1))
  expect_is(df2$cyl, "factor")
  expect_is(df3$cyl, "character")
})

test_that("bind_rows2 adds missing columns", {
  df1 <- df2 <- mtcars
  df2$cyl <- NULL
  df2$mpg <- NULL
  
  expect_error(bind_rows2(df1, df2, addMissing = FALSE))
  
  df3 <- bind_rows2(list(df1, df2), verbose = TRUE)
  
  
  expect_equal(nrow(df1), nrow(df2))
  expect_equal(nrow(df3), 2 * nrow(df1))
  expect_is(df2$cyl, "NULL")
  expect_is(df3$cyl, "numeric")
})

