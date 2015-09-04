#' Cross-validation of regression models
#' 
#' Generic function that computes a crossvalidation score for regression models. 
#' 
#' 
#' @param object a model object
#' @param kfolds Number of folds to use for crossvalidation. 0 (default) corresponds to leave-one-out crossvalidation.
#' @param statistic performance statistic to use.
#' 
#' @export

crossvalidate <- function(object, ...) {
  UseMethod("crossvalidate")
}

#' Ordinary Crossvalidation score
#' Following Wood (2006), p. 173
#' @param object a model object with methods for `residuals` and `hatvalues`.
#' @export
#' 
ocv <- function(object) {
  hat <- stats::hatvalues(goodlm)
  denom <- (1 - hat)^2
  ocv <- mean(residuals(object)^2 / denom)
  ocv
}

#' @export
#' @importFrom stats hatvalues
hatvalues.gam <- function(object) {
  object$hat
}


#' @export

crossvalidate.lm <- function(object, kfolds = 0, statistic = c("R2", "mse", "mae", "rmse")) {
  
  data <- getData(object)
  yname <- object$yname # sneaky back door for objects of my own design
  curcall <- object$call
  if(is.null(yname))
    yname <- as.character(curcall$formula[[2]])
  ymeas <- data[[yname]]

  statistic = match.arg(statistic)
  sfun = ifelse(statistic == "mae", "mae", "sse")
  
  if(kfolds == 0) { # leave-one-out crossvalidation
    if(statistic != "mae") {
      mse <- ocv(object)
      if (statistic == "mse")
        return(mse)
      else if (statistic == "rmse")
        return(sqrt(mse))
      else 
        return = 1 - mse / sum((ymeas - mean(ymeas, na.rm = TRUE))^2)
    }
    kfolds <- nrow(data)
  }
  
  # split data into folds
  case.folds = sample(rep(1:kfolds, length.out = nrow(data)))
  fold.stat = rep(NA_real_, kfolds) # mean absolute error for each fold
  
  for (fold in 1:kfolds) {
    train <- data[case.folds != fold, ]
    test <- data[case.folds == fold, ]
    curcall$data <- quote(train)
    curobj <- eval(curcall)
    
    ypred <- as.numeric(predict(curobj, newdata = test))
    ymeas <- test[[yname]]

    fold.stat[fold] <- do.call(sfun, list(ymeas, ypred))
  }
  
  # assemble folds 
  ymeas <- data[[yname]]
  ssum <- sum(fold.stat)
  
  if(statistic == "mae")
    sagg = ssum / kfolds
  else if (statistic == "mse")
    sagg = ssum / nrow(data)
  else if (statistic == "rmse")
    sagg = sqrt(ssum / nrow(data))
  else 
    sagg = 1 - ssum / sum((ymeas - mean(ymeas, na.rm = TRUE))^2)
  
  sagg
}


getMaker <- function(object) {
  c1 = class(object)[1]
  classes = c("lm", "glm", "gam", "rcgam")
  functions = c("lm", "glm", "gam", "rcgam")
  f <- functions[match(c1, classes)]
  f
}

getMakerEnv <- function(object) {
  c1 = class(object)[1]
  classes = c("lm", "glm", "gam", "rcgam")
  envs = c("stats", "stats", "gam", "rcgam")
  env <- environment()
  env
}
