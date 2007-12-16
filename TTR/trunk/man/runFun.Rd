\name{runFun}
\alias{runSum}
\alias{runMin}
\alias{runMax}
\alias{runSumProd}
\alias{runVar}
\title{ Analysis of Running/Rolling/Moving Windows }
\description{
  Various functions to analyze data over a moving window of periods.
}
\usage{
  runSum(x, n=10)
  runMin(x, n=10)
  runMax(x, n=10)
  runSumProd(x, y, n=10)
  runVar(x, n=10, sample=TRUE)
}
\arguments{
  \item{x}{ Object able to be coerced to a matrix. }
  \item{y}{ Object able to be coerced to a matrix. }
  \item{n}{ Number of periods to use in the window. }
  \item{sample}{ Logical, sample variance if \code{TRUE}, population otherwise. }
}
\details{
  \code{runSum} calculates sums over a moving window.

  \code{runMin} calculates minimums over a moving window.

  \code{runMax} calculates maximums over a moving window.

  \code{runSumProd} calculates sum products over a moving window.

  \code{runVar} calculates variance over a moving window.
}
\value{
  \code{runSum} returns sums over a n-period moving window.

  \code{runMin} returns minimums over a n-period moving window.

  \code{runMax} returns maximums over a n-period moving window.

  \code{runSumProd} returns sum products over a n-period moving window.

  \code{runVar} returns variance over a n-period moving window.
}
\author{ Josh Ulrich }
%\note{
%  In \code{growth} you can specify the number of periods and type of compounding
%  to use when calculating returns of the price series via the \code{'\dots'} argument.
%}
%--\examples{
%--##---- Should be DIRECTLY executable !! ----
%--##-- ==>  Define data, use random,
%--##--	or do  help(data=index)  for the standard data sets.
%--}
\keyword{ ts }
