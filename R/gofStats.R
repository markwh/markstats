
#' coefficient of determination
#' @param obs observed data
#' @param pred predictions
#' 
#' @export
R2 <- function(obs, pred) {
  stopifnot(length(obs) == length(pred))
  1 - var(obs - pred) / var(obs)
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