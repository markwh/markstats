# Functions for ggplot2. 


#' Termplot function for regression models
#' Currently only supports gam objects. 
#' @param ... Passed to facet_wrap
#' @export
#' 

ggTermPlot <- function(object, ...) {
  if(!requireNamespace("ggplot2", quietly = TRUE)){
    stop("ggplot2 must be installed for ggtermplot to work")
  }
  UseMethod("ggTermPlot")
}

#' @export
ggTermPlot.gam <- function(object, nrow = 1, ...) {
  dat = plot(object, pages = 1, residuals = TRUE)
  makeDf1 = function(elem) {
    df1 = data.frame(x = elem$x, y = elem$fit, what = "fit", xlab = elem$xlab)
    df1
  }
  makeDf2 = function(elem){
    df2 = data.frame(x = elem$raw, y = elem$p.resid, what = "p.resid", xlab = elem$xlab)
    df2
  }
  df1s = Reduce(rbind, lapply(dat, makeDf1))
  df2s = Reduce(rbind, lapply(dat, makeDf2))
  out = ggplot2::ggplot() +
    ggplot2::geom_line(data = df1s, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(data = df2s, ggplot2::aes(x = x, y = y), color = 2) +
    ggplot2::facet_wrap(~xlab, nrow = nrow, scales = "free_x", ...)
  out
}


#' Subsets a ggplot object by applying subset.data.frame code to gg$data
#' 
#' @export
subset.ggplot <- function(gg, subset, select, drop = FALSE) {
  x = gg[["data"]]
  r <- if (missing(subset)) 
    rep_len(TRUE, nrow(x))
  else {
    e <- substitute(subset)
    r <- eval(e, x, parent.frame())
    if (!is.logical(r)) 
      stop("'subset' must be logical")
    r & !is.na(r)
  }
  vars <- if (missing(select)) 
    TRUE
  else {
    nl <- as.list(seq_along(x))
    names(nl) <- names(x)
    eval(substitute(select), nl, parent.frame())
  }
  gg[["data"]] = x[r, vars, drop = drop]
  gg
}