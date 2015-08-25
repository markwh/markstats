# Functions for project organization, assuming use of ProjectTemplate package

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