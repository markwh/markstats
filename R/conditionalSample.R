#' Derive arguments to distribution functions from distribution moments
#'
#' Returns a list of distribution parameters for R distribution calls.
#'
#' @param distr character. A distribution R recognizes (currently only supports
#' 'norm', 'gamma', 'poisson')
#' @param moments a numeric vector, matrix, or data.frame with mean as column 1,
#' variance as column 2, etc. Currently only first 2 moments are used.
#' @return list of distribution parameters for R param calls.
#' @export

momentsToDistArgs = function(distr, moments) {
  if (is.vector(moments)) 
    moments = matrix(moments, nrow = 1)
  moments = as.matrix(moments)
  switch(distr, norm = list(mean = moments[, 1], sd = sqrt(moments[, 2])), 
         gamma = list(shape = moments[, 1]^2/moments[, 2], scale = moments[, 2]/moments[, 1]), 
         pois = list(lambda = moments[, 1]))
}


#' Generate samples from conditional distribution.
#'
#' @param object a model object with a `predict()` method
#' @param ... arguments to be passed to individual methods. These should always include :
#'  `newdata` - to be passed to `predict()` function
#'  `quantile` - either 'random' for stochastic sampling from conditional distribution or 
#'  a numeric value on (0, 1) specifying the quantile to return
#' @return Numeric vector containing conditional random sample (if `quantile = 'random'`)
#' or conditional quantiles from condition distribution defined by `object` and `newdata`
#' seealso \code{\link{condlSample.lm}}
#' @export

condlSample <- function(object, ...) {
  UseMethod("condlSample")
}

#' Generate samples from conditional distribution defined by a (generalized) linear model.
#'
#' @param object a lm object (including glm and gam). If a glm/gam, its family
#' must be gaussian, binomial, poisson, or gamma.
#' @param newdata As in predict.lm, predict.glm, etc. An optional data.frame to use for
#' generating conditional distribution parameters. If omitted, the fitted values are used.
#' @param quantile vector of quantiles to which the returned values will correspond.
#' If the default, 'random' is not used, this must be numeric on (0, 1).
#' @param ... Passed to `predict()` method
#' @return Numeric vector containing conditional random sample (if `quantile = 'random'`)
#' or conditional quantiles from condition distribution defined by `object` and `newdata`
#' @export

condlSample.lm <- function(object, newdata, quantile = "random", ...) {
  
  if (quantile == "random") 
    quantile = runif(nrow(newdata)) else quantile = rep_len(quantile, nrow(newdata))
    
    # get distribution from family(object)
    fam = stats::family(object)$family
    fams = c("binomial", "gaussian", "Gamma", "poisson")
    distrs = c("binom", "norm", "gamma", "pois")
    distr = distrs[which(fams == fam)]
    if (identical(distr, integer(0))) 
      stop("Specified distribution not available.")
    
    # get moments via predict
    preds = as.data.frame(predict(object = object, newdata = newdata, se.fit = TRUE, ...))
    preds$var.pred = preds$se.fit^2 + var(residuals(object, type = "response"))
    names(preds)[1] = "fit"
    
    # get distribution parameters via moments
    args0 = momentsToDistArgs(distr = distr, moments = preds[c("fit", "var.pred")])
    
    # sample from distribution
    funname = paste0("q", distr)
    out = do.call(funname, c(list(p = quantile), args0))
    out
}
