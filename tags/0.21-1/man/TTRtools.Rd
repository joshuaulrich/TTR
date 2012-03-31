\name{TTRtools}
\alias{growth}
\alias{lags}
\alias{wilderSum}
\title{ Miscellaneous Tools }
\description{
  Various functions that may be useful in designing technical trading rules.
}
\usage{
  growth(price, signals, \dots)
  lags(x, n=1)
  wilderSum(x, n=10)
}
\arguments{
  \item{price}{ Price series that is coercible to xts or matrix. }
  \item{signals}{ Signals to use (defaults to vector of ones).  Use '0' for no
    position, '1' for long position, and '-1' for short position. }
  \item{x}{ Object that is coercible to xts or matrix. }
  \item{n}{ Number of periods to use. }
  \item{\dots}{ Further arguments to be passed from or to other methods. }
}
\details{
  \code{growth} calculates the growth of an investment using given prices and signals.

  \code{lags} calculates the lags of a given series.

  \code{wilderSum} calculates a Welles Wilder style weighted sum.
}
\value{
  \code{growth} returns a vector of the growth of the investment.

  \code{lags} returns a matrix of lagged values of the original vector.

  \code{wilderSum} returns a vector of weighted sums.
}
\author{ Joshua Ulrich }
\note{
  In \code{growth} you can specify the number of periods and type of compounding
  to use when calculating returns of the price series via the \code{'\dots'} argument.
}
%--\examples{
%--##---- Should be DIRECTLY executable !! ----
%--##-- ==>  Define data, use random,
%--##--	or do  help(data=index)  for the standard data sets.
%--}
\keyword{ ts }
