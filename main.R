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
use_package("ggplot2", type = "Suggests")
use_package("fitdistrplus")
use_package("truncdist")
use_package("rcmodel", type = "Suggests")
use_testthat()

# datasets
set.seed(82)
x1 = rnorm(30); x2 = rnorm(30)
y1 = rnorm(30, 3, 0.5) + x1 - x2 / 4; y2 <- rnorm(30, 3, 0.5) + x1^2 - cos(x2)
fakedata <- data.frame(x1 = x1, x2 = x2, y1 = y1, y2  = y2)
goodlm <- lm(y1 ~ x1 + x2, fakedata)
use_data(goodlm, overwrite = TRUE)

goodgam <- mgcv::gam(y2 ~ s(x1) + s(x2))
use_data(goodgam, overwrite = TRUE)

# document
devtools::document()

# load
load_all()
install()

# test
test()


fixInNamespace("crossvalidate.lm", "markstats")
