# Functions for project organization, assuming use of ProjectTemplate package

#' Fetch a single object from cache
#' 
#' Does the opposite of ProjectTemplate::cache -- given an object name (as a
#' character string), retrieves that object from the cache directory.
#' 
#' @param ...	the objects to be retrieved, as names (unquoted) or character strings (quoted).
#' @param list	a character vector naming objects to be retrieved.
#' @param envir the environment into which to retrieve the object.
#' @return None, used for side-effects only
#' @seealso  ProjectTemplate::cache
#' @export

fromcache <- function(..., list = character(), envir = parent.frame()) {
  dots <- match.call(expand.dots = FALSE)$...
  if (length(dots) && !all(vapply(dots, function(x) is.symbol(x) || 
                                  is.character(x), NA, USE.NAMES = FALSE))) 
    stop("... must contain names or character strings")
  names <- vapply(dots, as.character, "")
  if (length(names) == 0L) 
    names <- character()
  list <- .Primitive("c")(list, names)
  files <- file.path("cache", paste0(list, ".RData"))
  lapply(files, load, envir = envir)
}