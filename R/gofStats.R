
#' coefficient of determination
#' 
#' @export
#' 
R2 <- function(x, ...) {
  UseMethod("R2")
}


#' Default S3 method for coefficient of determination
#' @param x observed data
#' @param xpred predictions
#' @export
R2.default <- function(x, xpred) {
  stopifnot(length(x) == length(xpred))
  1 - sum((x - xpred)^2) / sum((x - mean(x))^2)
}

#' Coefficient of dermination for lm objects
#' Optionally gives adjusted R2.
#' @param x an object of class "lm"
#' @export
R2.lm <- function(x, adjust = FALSE) {
  if(adjust)
    out <- summary(x)$adj.r.squared
  else
    out <- summary(x)$r.squared
  out
}

#' Coefficient of dermination for gam objects
#' Optionally gives adjusted R2.
#' @param x an object of class "gam"
#' @export
R2.gam <- function(x, adjust = FALSE) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("mgcv needed for this function to work. Please install it.",
         call. = FALSE)
  }
  resid <- mgcv::residuals.gam(x, type = "response")
  if(adjust)
    out <- summary(x)$r.sq
  else
    out <- 1 - sum(resid^2) / sum((x$y - mean(x$y))^2)
  out
}


#' Leave-one-out coefficient of determination
#' Similar to R2, but uses leave-one-out estimates for response and mean. 
#' Uses GCV and OCV to estimate MSE of prediction. 
#' @param object an object with a class for `deviance`, `residuals`, and `hatvalues`.
#' @export
#' 
Q2 <- function(object, method = c("gcv", "ocv")) {
  method = match.arg(method)
  mse <- ifelse(method == "gcv", gcv(object), ocv(object))
  1 - length(object$y) * mse / tss_loo(object$y)
}


#' mean absolute error
#' @param obs observed data
#' @param pred predictions
#' 
#' @export
mae <- function(obs, pred) {
  stopifnot(length(obs) == length(pred))
  mean(abs(obs - pred))
}

#' sum of squared errors
#' @param obs observed data
#' @param pred predictions
#' 
#' @export
sse <- function(obs, pred) {
  stopifnot(length(obs) == length(pred))
  sum((obs - pred)^2)
}

#' leave-one-out sum of squares
#' useful for calculating Q2
#' @export

tss_loo <- function(x) {
  omitted_mean <- function(ind) mean(x[-ind])
  omeans <- vapply(1:length(x), omitted_mean, numeric(1))
  sum((x - omeans)^2)
}



# gam functions --------------------------------------------------------------

#' @importFrom mgcv magic.post.proc
#' @param X A model matrix, potentially with unobserved prediction values
#' @param object a gam model

hatvalues.gam <- function(object, X = NULL) {
  if (is.null(X)) 
    return(object$hat)
  return(magic.post.proc(X = X, object = object)$hat)
}

#' Studentized deleted residuals, given a model object
#' @param object a model object that undertstands `residuals` and `hatvalues`
#' @export
rstudent.gam <- function(object, ...) {
  r <- delResid(object)
  s <- sqrt(delMSE(object) / (1 - hatvalues(object)))
  out <- r / s
  out
}

#' Deleted residuals, given a model object
#' @param object a model object that undertstands `residuals` and `hatvalues`
#' @export
delResid <- function(object) {
  residuals(object = object, type = "response") / (1 - hatvalues(object))
}

#' Mean-squared error for a model
#' @param model a model object that understands `residuals`
#' @export
MSE <- function(model) {
  errs <- residuals(model, type = "response")
  out <- sum(errs^2) / model$df.residual
  out
}

#' Deleted MSE
#' Useful for calculating deleted statistics like dffits, studentized deleted residuals
#' @param model object, e.g. lm or gam
#' @export
delMSE <- function(model) {
  n_p <- model$df.residual
  errs <- residuals(model, type = "response")
  numer <- MSE(model) * n_p - errs^2 / (1 - hatvalues(model))
  out <- numer / (n_p - 1)
  out
}

#' DFFITS for a given model
#' @export
dffits.gam <- function(model) {
  numer <- delResid(model) - residuals(model, type = "response")
  out <- numer / sqrt(delMSE(model) * hatvalues(model))
  out
}

