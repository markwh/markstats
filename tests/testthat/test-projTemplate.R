context("projectTemplate functions")

test_that("fromcache works for different environments", {
  isthere <- dir.exists("cache")
  if(!isthere) {
    dir.create("cache")
    on.exit(file.remove("cache"))
  }
  
  # put object in cache
  foo = "abc"
  save("foo", file = "cache/foo.RData")
  rm(foo)
  
  # test
#   browser()
  expect_false(exists("foo", envir = parent.frame()))
  fromcache("foo")
  expect_true(exists("foo", envir = parent.frame()))
  rm(foo)
  
  # delete file
  file.remove(file.path("cache", paste0("foo", ".RData")))
  if(!isthere) 
    file.remove("cache")
})