#' Erlang B Formula (aka. Erlang loss Formula)
#'
#' Returns the probability that an arriving customer is blocked.
#' @param c number of servers
#' @param lambda arrival rate
#' @param mu service rate
#'
#' @return Probability
#' @export
#'
Erlang_B <- function(c, lambda, mu){
  if (c == 1){
    return(1)
  }
  else{
    n <- c-1
    x <- lambda/mu
    B <- (x^n/factorial(n)) / (sum(x^(0:n) / factorial(0:n)))
    l <- (x*B)/(c+x*B)
    return(l)
  }
}
