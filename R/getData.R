#' Get data from lm objects
#' 
#' simple extraction of data, returning useful errors if impossible. Useful as a 
#' method for the generic `getData` from `nlme` package
#' 
#' @param object an object inheriting from class `lm`
#' @seealso getData.rcgam
#' @export
#' 

getData <- function(object, ...) {
  UseMethod("getData")
}

#' @export
getData.lm <- function(object) {
  if (is.null(object$model)) {
    message("model structure does not include data. Attempting to get from environment")
    eval(object$call$data, envir = attr(object$terms, ".Environment"))
  }
  else
    object$model
}
