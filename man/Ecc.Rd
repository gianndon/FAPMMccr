% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ecc.R
\name{Ecc}
\alias{Ecc}
\title{Expected Values}
\usage{
Ecc(c, lambda, mu, type = "")
}
\arguments{
\item{c}{number of servers}

\item{lambda}{arrival rate}

\item{mu}{service rate}

\item{type}{character, type in E_Nq, E_N, E_Wq, E_W for the desired expected value}
}
\value{
Expected Values either of E_Nq, E_N, E_Wq, E_W
}
\description{
Returns the expected value depending on the entry for type
}
