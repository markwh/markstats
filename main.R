# construction and maintenance for markstats package
# Mark Hagemann
# 8/4/2015


library(devtools)
library(testthat)

# setup
use_package("assertthat")
use_package("testthat")
use_package("dplyr")
use_package("nlme", type = "Depends")
use_testthat()

# load
load_all()
install()

# test
test()
