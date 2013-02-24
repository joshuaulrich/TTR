\name{rollSFM}
\alias{rollFun}
\alias{rollSFM}
\title{Analysis of Running/Rolling/Moving Windows}
\usage{
  rollSFM(Ra, Rb, n = 60)
}
\arguments{
  \item{Ra}{Object coercible to xts or matrix, containing
  the excess return for an individual security}

  \item{Rb}{Object coercible to xts or matrix, containing
  the market / benchmark return}

  \item{n}{Number of periods to use in the window}
}
\value{
  A object of the same class as \code{Ra} (and \code{Rb}?)
  or a vector (if \code{try.xts} fails). \describe{
  \item{rollSFM}{returns single-factor model parameters and
  R-squared over a n-period moving window.} }
}
\description{
  Various functions to analyze data over a moving window of
  periods.
}
\author{
  Joshua Ulrich
}
\references{
  The following site(s) were used to code/document this
  indicator:
  \url{http://en.wikipedia.org/wiki/Simple_linear_regression}\cr
}
\keyword{ts}

