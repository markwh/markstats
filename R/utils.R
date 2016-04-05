# Various utility functions

#' Fetch a single object from cache
#' 
#' Does the opposite of ProjectTemplate::cache -- given an object name (as a
#' character string), retrieves that object from the cache directory.
#' 
#' @param variable character, the name of the variable to be retrieved.
#' @param value None, used for side-effects only
#' @seealso ProjectTemplate::cache
#' @export


fromcache <- function(variable, envir = parent.frame()) {
  load(file.path("cache", paste0(variable, ".RData")), envir = envir)
}


#' Get all of the pieces for a particular stringsplit.
#' @param strvec Vector of strings, each to be split.
#' @param split The string to split on. See `help("strsplit")`
#' @param piece The integer location of the string split piece desired
#' @param ... Other arguments passed to `strsplit()`
#' @export
splitPiece <- function(strvec, split, piece, ...) {
  spl <- strsplit(strvec, split = split, ...)
  out <- vapply(spl, `[`, character(1), piece)
  out
}

#' Capitalize first letter of each word in text
#' Copied without shame from http://stackoverflow.com/a/15776815
#' @export

titleCase <- function(text) {
  re_from <- "\\b([[:lower:]])([[:lower:]]+)"
  out <- gsub(re_from, "\\U\\1\\L\\2" , text, perl=TRUE)
  out
}
