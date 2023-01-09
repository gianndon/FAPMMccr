#' Expected Values
#'
#' Returns the expected value depending on the entry for type
#' @param c number of servers
#' @param lambda arrival rate
#' @param mu service rate
#' @param type character, type in E_Nq, E_N, E_Wq, E_W for the desired expected value
#'
#' @return Expected Values either of E_Nq, E_N, E_Wq, E_W
#' @export
#'
Ecc <- function(c, lambda, mu, type=""){

  # Berechnung Erlang B Formel
  EB <- function(c, lambda, mu){
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

  # Aufruf der Erlang B Formel
  pc <- EB(c=c, lambda=lambda, mu=mu)

  # Rückgabe der Erwartungswerte abhängig von der eingabe bei type
  if (type == "E_Nq"){
    return(0)
  }
  else if (type == "E_N"){
    return((1-pc)*lambda/mu)
  }
  else if (type == "E_Wq"){
    return(0)
  }
  else if (type == "E_W"){
    return(1/mu)
  }
  else if (type == ""){
    print("Please enter E_Nq, E_N, E_Wq or E_W for type!", quote=FALSE)
  }
  else{
    print("Wrong entry! Please enter E_Nq, E_N, E_Wq or E_W for type!", quote=FALSE)
  }
}
