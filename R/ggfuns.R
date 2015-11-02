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
  png("tmp.png")
  dat = plot(object, pages = 1, residuals = TRUE)
  dev.off()
  file.remove("tmp.png")
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
