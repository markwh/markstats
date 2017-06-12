# Various utility functions


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


#' Wrapper for summary(as.factor(x))
#' @export

facsum <- function(x) 
  summary(as.factor(x))


addVars <- function(df, names, verbose = FALSE) {
  if (length(names) == 0)
    return(df)
  
  newcols <- matrix(NA, nr = nrow(df), nc = length(names)) %>% 
    as.data.frame() %>% 
    setNames(names)
  
  if (verbose) 
    message(sprintf("Adding the following columns: %s", 
                    paste(names, collapse = ", ")))
  
  out <- cbind(df, newcols)
  out
}

#' Used in bind_rows2
#' 
#' Converts conflicted column types to character columns and rbinds the result. 
#' If one data.frame is missing columns, they are created and populated by NA's. 
#' 
#' @param df1 data.frame (or similar) to combine
#' @param df2 data.frame (or similar) to combine
#' @param addMissing Add columns to result if they are missing in either df1 or df2?
#' @param verbose Include messages about conversions?
#' 
#' @export

conflictsToCharacter <- function (df1, df2, addMissing = TRUE, verbose = FALSE) 
{
  df1 <- as.data.frame(df1)
  df2 <- as.data.frame(df2)
  
  allcols <- union(names(df1), names(df2))
  newTo1 <- setdiff(allcols, names(df1))
  newTo2 <- setdiff(allcols, names(df2))
  
  if (addMissing) {
    df1 <- addVars(df1, newTo1, verbose = verbose)[allcols]
    df2 <- addVars(df2, newTo2, verbose = verbose)[allcols]
  } else if (length(newTo1) > 0 || length(newTo2) > 0)
    stop("Column names must match unless addMissing is TRUE")
  
  conflicts = !unlist(Map(identical, lapply(df1, class), 
                          lapply(df2, class)))
  conflicts[c(newTo1, newTo2)] <- FALSE
    
  if (sum(conflicts) > 0 && verbose) 
    message(paste("Converting to character:", names(conflicts)[conflicts]))
  df1[conflicts] = lapply(df1[conflicts], as.character)
  df2[conflicts] = lapply(df2[conflicts], as.character)
  rbind(df1, df2)
}

#' A better (but slower) version of dplyr::bind_rows
#' 
#' @param dflist a list of data.frames
#' 
#' @export
#' 

bind_rows2 <- function (dfList, addMissing = TRUE, verbose = FALSE) {
  redfun <- function(x, y) 
    conflictsToCharacter(x, y, addMissing = addMissing, verbose = verbose)
  out <- Reduce(redfun, dfList)
  out
}
  