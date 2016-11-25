# Functions for ggplot2. 


#' ggplot2 implementation of termplot with partial residuals, using visreg::visreg()
#' @param object A model object that visreg() recognizes
#' @param xvar Which term to inspect. Currently only works for one at a time.
#' @param data If object$call$data isn't in the model environment, it can be supplied using this
#' @importFrom visreg visreg
#' @importFrom ggplot2 ggplot geom_ribbon geom_line geom_point aes_
#' @export
ggTermPlot <- function(object, ...) {
  UseMethod("ggTermPlot")
}

#' @export
ggTermPlot.lm <- function(object, xvar, data = NULL, ...) {
  if (!is.null(data))
    object$call$data <- as.name("data")
  vis = visreg(object, xvar = xvar, plot = FALSE, ...)
  
  out <- ggplot(data = vis$fit) + 
    geom_ribbon(aes_(x = as.name(xvar), ymin = ~visregLwr, ymax = ~visregUpr), 
                alpha = 0.5) +
    geom_line(aes_(x = as.name(xvar), y = ~visregFit)) + 
    geom_point(aes_(x = as.name(xvar), y = ~visregRes), data = vis$res)
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


 
#' Creates 2-d filled contour plot for relative differences between two 
#' data frames, each with 2 columns, "Date" and a numeric value.
#' Series 1 is reference, to which series2 is compared.
#' Value plotted is percent difference

relDif = function(series1, series2, log = T, breaks = NULL, cols = NULL){
  # 
  # Creates 2-d filled contour plot for relative differences between two 
  #  data frames, each with 2 columns, "Date" and a numeric value.
  #  Series 1 is reference, to which series2 is compared
  #  Value plotted is percent difference
  #
  require(caTools)
  
  daysOfInterest = c(1, 7, 14, 30, 60, 90, 180, 365)
  base = 1.05
  daysForLoad = base ^ (0:ceiling(log(365, base = base)))
  
  data = merge(series2, series1, by = "Date", all = F)
  data$dif = data[,2] - data[,3]
  outmat = matrix(nrow = 0, ncol = nrow(data))
  for(i in daysForLoad){
    avdif = runmean(data$dif, k = round(i), align = "center", endrule = "NA")
    avload = runmean(data[,3], k = round(i), align = "center", endrule = "NA")
    reldif = avdif / avload * 100
    outmat = rbind(outmat, reldif)
  } # get running mean for different window sizes
  rownames(outmat) = daysForLoad
  colnames(outmat) = data$Date
  
  date1 = data$Date[1]
  date2 = data$Date[nrow(data)]
  dates = seq.Date(date1, date2, by = "month")
  
  #   outmat[is.na(outmat)] = -110
  #   cols = c("black", cm.colors(19), "red")
  # cols = c("black", rainbow(19), "black")
  if (is.null(cols)) {
    cols <- c("black", rainbow(19), "black")
    cols[11] = "lightgray"
  }
  if (is.null(breaks))
    breaks <- c(-10:-1 * 10, 1:10 * 10)
  stopifnot(all(breaks >= -100))
  stopifnot(length(breaks) == length(cols) - 1)
  allbreaks = c(-110, breaks, max(110, ceiling(max(outmat, na.rm = T))))
  
  par.old = par(no.readonly = T)
  #   par(bg = "gray")
  if(log){
    # log plot
    filled.contour(as.numeric(data$Date), log(daysForLoad), t(outmat), 
                   levels = allbreaks, 
                   xlab = "Date", ylab = "length of load time period (days)",
                   col = cols, key.title = title(main = "percent\ndif."),
                   plot.axes = { axis(1, at = as.numeric(dates), 
                                      labels = format(dates, "%m/%Y"))
                     axis(2, at = log(daysOfInterest), 
                          labels = daysOfInterest) })
    #                    plot.title = title(main = paste(series1, series2)))  
  } else {
    #linear plot
    filled.contour(as.numeric(data$Date), daysForLoad, t(outmat), 
                   levels = allbreaks, 
                   xlab = "Date", ylab = "length of load time period (days)",
                   col = cols, key.title = title(main = "percent\ndif."),
                   plot.axes = { axis(1, at = as.numeric(dates),
                                      labels = format(dates, "%m/%Y"))
                     axis(2, at = daysOfInterest, 
                          labels = daysOfInterest) })
    #                    plot.title = title(main = paste(series1, series2)))  
  }
  par(par.old)
  invisible(outmat)
}


# Multivariate sampling and plotting functions ----------------------------


#' Add points corresponding to a multivariate normal sample
#' @param mvsample a data.frame returned by mvSample()
#' @param ... Arguments passed to geom_point
#' @importFrom ggplot2 geom_point
#' @export
ggMvSample <- function(mvsample, ...) {
  geom_point(data = mvsample, aes(x = x, y = y), ...)
}

#' Returns a data.frame with points sampled according to a covariance matrix
#' Only works for 2-D data
#' @param sigma a 2x2 covariance matrix
#' @param if not "none" (the default), uses a quasi-random sampling method
#' @param ... other arguments passed to halton() or sobol() (if quasi != "none")
#' @importFrom randtoolbox halton sobol
#' @importFrom magrittr "%>%"
#' @export
mvSample <- function(sigma, n, quasi = c("none", "halton", "sobol"), ...) {
  
  stopifnot(nrow(sigma) == 2 && ncol(sigma) == 2)
  eigs <- eigen(sigma)
  stopifnot(all(eigs$values > 0))
  quasi <- match.arg(quasi)
  
  if(quasi == "halton") {
    mv1 <- halton(n = n, dim = 2, normal = TRUE, ...)
    mvdat <- mv1 %*% chol(sigma) %>% 
      as.data.frame() %>% 
      setNames(c("x", "y"))
  }
  if(quasi == "sobol") {
    mv1 <- sobol(n = n, dim = 2, normal = TRUE, ...)
    mvdat <- mv1 %*% chol(sigma) %>% 
      as.data.frame() %>% 
      setNames(c("x", "y"))
  }
  if(quasi == "none") {
    mvdat <- rmvnorm(n = n, sigma = sigma) %>% 
      as.data.frame() %>% 
      setNames(c("x", "y"))
  }
  mvdat
}

#' Returns a data.frame with points sampled along first pincipal component of a covariance matrix
#' Only works for 2-D data
#' @param sigma a 2x2 covariance matrix
#' @param n number of points in sample
#' @param if not "none" (the default), uses a quasi-random sampling method
#' @importFrom randtoolbox halton sobol
#' @importFrom magrittr "%>%"
eigSample <- function(sigma, n, quasi = c("none", "halton", "sobol"), ...) {
  
  stopifnot(nrow(sigma) == 2 && ncol(sigma) == 2)
  eigs <- eigen(sigma)
  stopifnot(all(eigs$values > 0))
  quasi <- match.arg(quasi)
  
  if(quasi == "halton") {
    mv1 <- halton(n = n, dim = 1, normal = TRUE, ...)
  }
  if(quasi == "sobol") {
    mv1 <- sobol(n = n, dim = 1, normal = TRUE, ...)
  }
  if(quasi == "none") {
    mv1 <- rnorm(n = n)
  }
  
  mvdat <- mv1 %>% 
    as.data.frame()
  out <- sampleToPCs(mvdat, sigma = sigma) %>% 
    as.data.frame() %>% 
    setNames(c("x", "y"))
  out
}

#' Project observations from a sample to principal components, given a covariance matrix
#' @param x A data.frame, typically a (quasi-) random sample from MVN(0, I)
sampleToPCs <- function(x, sigma) {
  eigs <- eigen(sigma)
  stopifnot(all(eigs$values > 0))
  dim <- ncol(x)
  stopifnot(dim <= ncol(sigma))
  lambdas <- eigs$values[1:dim]
  vmat <- eigs$vectors[, 1:dim]
  
  out <- as.matrix(x) %*% diag(sqrt(lambdas), nrow = dim, ncol = dim) %*% t(vmat)
  out
}

#' Returns a geom_segment object with principal components shown
#' @param pclines a data.frame returned by pcLines function
#' @param ... Arguments passed to geom_line
#' @importFrom ggplot2 geom_segment
ggPcLines <- function(pclines, ...) {
  geom_segment(data = pclines, aes(x = x_1, y = y_1, xend = x_2, yend = y_2), ...)
}

#' Returns a data.frame with endpoints of principal components.
#' Only works for 2-D data
#' @param pclines a data.frame returned by pcLines function
pcLines <- function(sigma) {
  stopifnot(nrow(sigma) == 2 && ncol(sigma) == 2)
  eigs <- eigen(sigma)
  stopifnot(all(eigs$values > 0))
  
  segends <- with(eigs, Map(`*`, as.data.frame(vectors), 2 *sqrt(values))) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    setNames(c("x", "y")) %>% 
    mutate(endpoint = -0.5, pc = c(1, 2)) %>% 
    bind_rows(-I(.)) %>% 
    mutate(endpoint = endpoint + 1.5, 
           pc = abs(pc)) %>%
    melt(id.vars = c("endpoint", "pc")) %>% 
    dcast(pc ~ variable + endpoint)
  segends
}
