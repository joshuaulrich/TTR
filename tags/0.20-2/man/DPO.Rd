\name{DPO}
\alias{DPO}
\title{ De-Trended Price Oscillator }
\description{
  The Detrended Price Oscillator (DPO) removes the trend in prices - or other series -
  by subtracting a moving average of the price from the price.
}
\usage{
  DPO(x, n=10, maType, shift=n/2+1, percent=FALSE, \dots)
}
\arguments{
  \item{x}{ Price, volume, etc. series that is coercible to xts or matrix. }
  \item{n}{ Number of periods for moving average. }
  \item{maType}{ A function or a string naming the function to be called. }
  \item{shift}{ The number of periods to shift the moving average. }
  \item{percent}{ logical; if \code{TRUE}, the percentage difference between the slow and fast moving
    averages is returned, otherwise the difference between the respective averages is returned. }
  \item{\dots}{ Other arguments to be passed to the \code{maType} function. }
}
\details{
  The Detrended Price shows cycles and overbought / oversold conditions. Note the calculation
  shifts the results \code{shift} periods, so the last \code{shift} periods will be zero.
}
\value{
  A object of the same class as \code{x} or a vector (if \code{try.xts}
  fails) containing the DPO values.
}
\author{ Joshua Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/DPO.htm}\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=48}\cr
}
\note{
  As stated above, the DPO can be used on any univariate series, not just price.
}
\seealso{
  See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average options; and note
  Warning section.  See \code{\link{MACD}} for a general oscillator.
}
\examples{
  data(ttrc)
  priceDPO <- DPO(ttrc[,"Close"])
  volumeDPO <- DPO(ttrc[,"Volume"])
}
\keyword{ ts }
