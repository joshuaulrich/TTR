\name{runPercentRank}
\alias{runPercentRank}
\alias{percentRank}
\alias{PercentRank}
\title{ Percent Rank over a Moving Window }
\description{
  This function computes a running/rolling percentage rank.
}
\usage{
  runPercentRank(x, n=260, cumulative=FALSE, exact.multiplier=0.5)
}
\arguments{
  \item{x}{ Object coercible to xts or matrix. }
  \item{n}{ Number of periods to use in the window or, if \code{cumulative=TRUE},
    the number of obversations to use before the first result is returned. }
  \item{cumulative}{ Logical, use from-inception calculation? }
  \item{exact.multiplier}{ The weight applied to identical values in the window. See details. }
}
\value{
  A object of percent ranks over a n-period moving window of the same class as \code{x} and \code{y} or a vector
  (if \code{try.xts} fails).
}
\details{ 
  The computation for a percentage rank can vary depending on the weight given to values in the window identical to the value being ranked.
  This weight can be set using the \code{exact.multiplier} argument which defaults to 0.5. 
}
\note{
  It may be important to note that this computation is different from the one used in Microsoft Excel's PERCENTRANK formula. 
  Excel's computation is rather strange and gives inconsistent results as it uses interpolation to rank values that are not found within the lookback window.
}
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://en.wikipedia.org/wiki/Percentile_rank}\cr
}
\author{ Charlie Friedemann }
\keyword{ ts }
