#' Pn Function
#'
#' Probability Pn of having n customers in the System
#' @param c number of server
#' @param mu service rate
#' @param lambda arrival rate
#' @param n number of subjects/objects in the system
#'
#' @return Probability
#' @export
#'
Pncc <- function(c, mu, lambda, n){

  # Berechnung u
  u <- lambda/(c*mu)

  # Berechnung p0
  p0 <- 1/sum((c*u)^(0:c)/factorial(0:c))

  # Rückgabe je nach Grösse n

  if (n == 0){
    return(p0)
  }
  else if (n >= 1 & n <= c){
    return(((c*u)^n/factorial(n))*p0)
  }
  else if (n >= c+1){
    return(0)
  }
  else{
    print("n can't be a negative number!", quote=FALSE)
  }
}





