\name{CMF}
\alias{CMF}
\title{ Chaikin Money Flow }
\description{
  Chaikin Money Flow compares total volume over the last \code{n} time periods to total volume
  times the Close Location Value (CLV) over the last \code{n} time periods.  Developed by Marc
  Chaikin.
}
\usage{
  CMF(HLC, volume, n = 20)
}
\arguments{
  \item{HLC}{ Object that is coercible to xts or matrix and contains High-Low-Close prices. }
  \item{volume}{ Vector or matrix of volume observations corresponding to the \code{HLC} object. }
  \item{n}{ Number of periods to use. }
}
\details{
  Chaikin Money Flow is calculated by taking dividing the sum of the Chaikin Accumulation /
  Distribution line over the past \code{n} periods by the sum of volume over the past
  \code{n} periods.
}
\value{
  A object of the same class as \code{HLC} and \code{volume} or a vector
  (if \code{try.xts} fails) containing the Chaikin Money Flow values.
}
\author{ Joshua Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/ChaikinMoneyFlow.htm}\cr
  \url{http://www.linnsoft.com/tour/techind/cmf.htm}\cr
  \url{http://stockcharts.com/education/IndicatorAnalysis/indic_ChaikinMoneyFlow1.html}\cr
}
\note{
  When Chaikin Money Flow is above/below +/- 0.25 it is a bullish/bearish signal.
  If Chaikin Money Flow remains below zero while the price is rising, it indicates a 
  probable reversal.
}
\seealso{
  See \code{\link{CLV}}, and \code{\link{chaikinAD}}.
}
\examples{
  data(ttrc)
  cmf <- CMF(ttrc[,c("High","Low","Close")], ttrc[,"Volume"])
}
\keyword{ ts }
