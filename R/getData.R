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

getData.lm <- function(object) {
  if (is.null(object$model)) {
    warning("model structure does not include data. Attempting to get from environment")
    eval(object$call$data, envir = attr(object$terms, ".Environment"))
  }
  else
    object$model
}


#' Get data from rcgam objects
#'
#' simple extraction of data, returning useful errors if impossible. Useful as a
#' method for the generic `getData` from `nlme` package
#'
#' @param object an object of class `rcgam`
#' @param type What kind of data to return--raw or transformed (rcData object)
#' @export

getData.rcgam <- function(object, type = c("raw", "rcData")) {
  type = match.arg(type)
  
  if (!requireNamespace("rcmodel", quietly = TRUE)) {
    stop("rcmodel needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  out <- if (is.null(object$data)) {
    warning("model structure does not include data. Attempting to get from environment")
    eval(object$call$data, envir = attr(object$terms, ".Environment"))
  }
  else
    object$data
  
  if(type == "raw")
    out <- rcmodel::makeRawData(out)
  out
}

#' @export
`[.rcData` <- function(x, i, ...) {
  r <- NextMethod("[")
  ats <- attributes(x)
  ats$names = names(r)
  ats$row.names = 1:nrow(r)
  mostattributes(r) <- ats
  r
}