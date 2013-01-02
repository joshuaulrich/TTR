\name{lags}
\alias{growth}
\alias{lags}
\alias{naCheck}
\alias{wilderSum}
\title{Miscellaneous Tools}
\usage{
  lags(x, n = 1)

  growth(price, signals, ...)

  naCheck(x, n = 0)
}
\arguments{
  \item{price}{Price series that is coercible to xts or
  matrix.}

  \item{signals}{Signals to use (defaults to vector of
  ones).  Use '0' for no position, '1' for long position,
  and '-1' for short position.}

  \item{x}{Object that is coercible to xts or matrix.}

  \item{n}{Number of periods to use.}

  \item{\dots}{Further arguments to be passed from or to
  other methods.}
}
\value{
  \code{growth} returns a vector of the growth of the
  investment.

  \code{lags} returns a matrix of lagged values of the
  original vector.

  \code{wilderSum} returns a vector of weighted sums.
}
\description{
  Various functions that may be useful in designing
  technical trading rules.
}
\details{
  \code{growth} calculates the growth of an investment
  using given prices and signals.

  \code{lags} calculates the lags of a given series.

  \code{wilderSum} calculates a Welles Wilder style
  weighted sum.
}
\note{
  In \code{growth} you can specify the number of periods
  and type of compounding to use when calculating returns
  of the price series via the \code{'\dots'} argument.
}
\author{
  Joshua Ulrich
}
\keyword{ts}

