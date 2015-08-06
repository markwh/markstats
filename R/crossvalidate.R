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


#' @export

crossvalidate.lm <- function(object, kfolds = 0, statistic = c("R2", "mse", "mae", "rmse")) {
  
  data <- getData(object)
  if(kfolds == 0) 
    kfolds <- nrow(data)
  curcall <- object$call
  
  yname <- object$yname # sneaky back door for objects of my own design
  if(is.null(yname))
    yname <- as.character(curcall$formula[[2]])
  
  yvar <- var(data[[yname]])
  
  statistic = match.arg(statistic)
  sfun = ifelse(statistic == "mae", "mae", "sse")
  
  # split data into folds
  case.folds = sample(rep(1:kfolds, length.out = nrow(data)))
  # fold.ss = rep(NA_real_, kfolds) # sum of squared residuals for each fold
  # fold.mae = rep(NA_real_, kfolds) # mean absolute error for each fold
  fold.stat = rep(NA_real_, kfolds) # mean absolute error for each fold
  
  for (fold in 1:kfolds) {
    train <- data[case.folds != fold, ]
    test <- data[case.folds == fold, ]
    curcall$data <- quote(train)
    curobj <- eval(curcall)
    
    ypred <- as.numeric(predict(curobj, newdata = test))
    ymeas <- test[[yname]]
    
    # fold.ss[fold] <- sum((ymeas - ypred)^2)
    fold.stat[fold] <- do.call(sfun, list(ymeas, ypred))
  }
  
  # assemble folds 
  # r2 <- 1 - sum(fold.ss) / sum((ymeas - mean(ymeas, na.rm = TRUE))^2)
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