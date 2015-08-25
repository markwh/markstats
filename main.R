# construction and maintenance for markstats package
# Mark Hagemann
# 8/4/2015


library(devtools)
library(testthat)
library(dplyr)

# setup
use_package("assertthat")
use_package("testthat")
use_package("dplyr")
# use_package("hydroGOF")
use_package("nlme", type = "Suggests")
use_package("ProjectTemplate", type = "Suggests")
use_package("fitdistrplus")
use_package("truncdist")
use_testthat()

# datasets
set.seed(82)
x1 = rnorm(30); x2 = rnorm(30); y = rnorm(30, 3, 0.5) + x1 - x2 / 4
fakedata <- data.frame(x1 = x1, x2 = x2, y = y)
goodlm <- lm(y ~ x1 + x2, fakedata)
use_data(goodlm, overwrite = TRUE)

# document
devtools::document()

# load
load_all()
install()

# test
test()


fixInNamespace("crossvalidate.lm", "markstats")
