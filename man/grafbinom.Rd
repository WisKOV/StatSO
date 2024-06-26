% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VerschillendeFuncties.R
\name{grafbinom}
\alias{grafbinom}
\title{grafbinom}
\usage{
grafbinom(size, prob)
}
\arguments{
\item{size}{waarde van de parameter n (natuurlijk getal) horend bij de binomiale verdeling = aantal pogingen}

\item{prob}{waarde van parameter p (0<p<1) horend bij de binomiale verdeling = kans op succes}
}
\value{
grafiek (staafdiagram)
}
\description{
Functie tekent de grafiek (staafdiagram) van de discrete kansverdeling bij een binomiale verdeling.
}
\examples{
grafbinom(10,0.4)
}
