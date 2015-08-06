
R2 <- function(obs, pred) {
  stopifnot(length(obs) == length(pred))
  1 - var(obs - pred) / var(obs)
}

mae <- function(obs, pred) {
  stopifnot(length(obs) == length(pred))
  mean(abs(obs - pred))
}

sse <- function(obs, pred) {
  stopifnot(length(obs) == length(pred))
  sum((obs - pred)^2)
}