\name{ttr.tools}
\alias{growth}
\alias{roll.fn}
\alias{lags}
\alias{wilder.sum}
\title{ Miscellaneous Tools }
\description{
  Various functions that may be useful in designing technical trading rules.
}
\usage{
  growth(price, signals, ...)
  roll.fn(x, n, FUN, ...)
  lags(x, n = 1)
  wilder.sum(x, n = 10)
}
\arguments{
  \item{price}{ Price series to use. }
  \item{signals}{ Signals to use (defaults to vector of ones).  Use \sQuote{0} for no
    position, \sQuote{1} for long position, and \sQuote{-1} for short position. }
  \item{x}{ Object able to be coerced to a matrix. }
  \item{n}{ Number of periods to use. }
  \item{FUN}{ Function to calculate over rolling subsets of data. }
  \item{...}{ Further arguments to be passed from or to other methods. }
}
\details{
  \code{growth} calculates the growth of an investment using given prices and signals.

  \code{roll.fn} allows the calculation of rolling functions.

  \code{lags} calculates the lags of a given series.

  \code{wilder.sum} calculates a Welles Wilder style weighted sum.
}
\value{
  \code{growth} returns a vector of the growth of the investment.

  \code{roll.fn} returns a vector of function results for the rolling subsets of data.

  \code{lags} returns a matrix of lagged values of the original vector.

  \code{wilder.sum} returns a vector of weighted sums.
}
\author{ Josh Ulrich }
\note{
  In \code{growth} you can specify the number of periods and type of compounding
  to use when calculating returns of the price series via the \sQuote{\dots} argument.
}
%--\examples{
%--##---- Should be DIRECTLY executable !! ----
%--##-- ==>  Define data, use random,
%--##--	or do  help(data=index)  for the standard data sets.
%--}
\keyword{ ts }
