\name{runFun}
\alias{runFun}
\alias{runSum}
\alias{runMin}
\alias{runMax}
\alias{runMean}
\alias{runMedian}
\alias{runCov}
\alias{runCor}
\alias{runVar}
\alias{runSD}
\alias{runMAD}
\title{ Analysis of Running/Rolling/Moving Windows }
\description{
  Various functions to analyze data over a moving window of periods.
}
\usage{
  runSum(x, n=10, cumulative=FALSE)
  runMin(x, n=10, cumulative=FALSE)
  runMax(x, n=10, cumulative=FALSE)
  runMean(x, n=10, cumulative=FALSE)
  runMedian(x, n=10, non.unique="mean", cumulative=FALSE)
  runCov(x, y, n=10, use="all.obs", sample=TRUE, cumulative=FALSE)
  runCor(x, y, n=10, use="all.obs", sample=TRUE, cumulative=FALSE)
  runVar(x, y=NULL, n=10, sample=TRUE, cumulative=FALSE)
  runSD(x, n=10, sample=TRUE, cumulative=FALSE)
  runMAD(x, n=10, center=NULL, stat="median",
         constant=1.4826, non.unique="mean", cumulative=FALSE)
}
\arguments{
  \item{x}{ Object coercible to xts or matrix. }
  \item{y}{ Object coercible to xts or matrix. }
  \item{n}{ Number of periods to use in the window or, if \code{cumulative=TRUE},
    the number of obversations to use before the first result is returned. }
  \item{cumulative}{ Logical, use from-inception calculation? }
  \item{sample}{ Logical, sample covariance if \code{TRUE} (denominator of \code{n-1}) }
  \item{use}{ Only \code{"all.obs"} currently implemented. }
  \item{non.unique}{ One of 'mean', 'max', or 'min'; which compute their respective
    statistics for the two middle values of even-sized samples. }
  \item{center}{ The values to use as the measure of central tendency, around
    which to calculate deviations. The default (\code{NULL}) uses the median. }
  \item{stat}{ Statistic to calculate, one of 'median' or 'mean' (e.g. median
    absolute deviation or mean absolute deviation, respectively.) }
  \item{constant}{ Scale factor applied to approximate the standard deviation. }
}
%\details{
%  \code{runSum} calculates sums over a moving window.
%  \code{runMin} calculates minimums over a moving window.
%  \code{runMax} calculates maximums over a moving window.
%  \code{runMean} calculates means over a moving window (provided for convenience).
%  \code{runMedian} calculates medians over a moving window.
%  \code{runCov} calculates covariances over a moving window.
%  \code{runCor} calculates correlations over a moving window.
%  \code{runVar} calculates variances over a moving window.
%  \code{runSD} calculates standard deviations over a moving window (provided for convenience).
%  \code{runMAD} calculates median/mean absolute deviations over a moving window.
%}
\value{
  A object of the same class as \code{x} and \code{y} or a vector
  (if \code{try.xts} fails).
  \item{runSum}{returns sums over a n-period moving window.}
  \item{runMin}{returns minimums over a n-period moving window.}
  \item{runMax}{returns maximums over a n-period moving window.}
  \item{runMean}{returns means over a n-period moving window.}
  \item{runMedian}{returns medians over a n-period moving window.}
  \item{runCov}{returns covariances over a n-period moving window.}
  \item{runCor}{returns correlations over a n-period moving window.}
  \item{runVar}{returns variances over a n-period moving window.}
  \item{runSD}{returns standard deviations over a n-period moving window.}
  \item{runMAD}{returns median/mean absolute deviations over a n-period moving window.}
}
\author{ Joshua Ulrich }
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
