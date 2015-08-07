#' Impute BDL data using various distributions
#' 
#' Function to impute BDL data, using either uniform or truncated lognormal distributions
#' The latter comes from the fitdistrplus::fitdistcens function
#' @param lrdf data.frame with columns "left" and "right"
#' @return  vector with length nrow(lrdf) that contains original and imputed values
#' THIS VERSION RETURNS NA when distribution cannot be fit. An earlier version used uniform when this happened.
#' 
#' 


randlr = function(lrdf, distr = c("unif", "norm", "lnorm")){

  stopifnot(is.data.frame(lrdf) && "left" %in% names(lrdf) && "right" %in% names(lrdf))
  lrdf2 = lrdf[c("left", "right")]
  lrdf$imputed = NA; lrdf$imp.dist = NA
  distr = match.arg(distr)
  
  iscen = lrdf2$left != lrdf2$right
  x = lrdf2$right

  dst = try(fitdistrplus::fitdistcens(lrdf2, distr))
  if(is(dst, "try-error")) return(lrdf)
  lc = lrdf[iscen, ]
  rtrunc1 = function(a, b) do.call("rtrunc", envir = environment(truncdist::rtrunc),
                                   c(list(n = 1, spec = distr, a = a, b = b),
                                     as.list(dst$estimate)))
  rtrunc1(0, 99)
  x[iscen] = unlist(Map(rtrunc1, a = lc$left, b = lc$right))
  
  lrdf$imputed = x
  lrdf$imp.dist = distr
  lrdf
}


