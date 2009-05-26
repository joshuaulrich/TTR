\name{ZigZag}
\alias{ZigZag}
\alias{zigzag}
\title{ Zig Zag }
\concept{trend}
\description{
  Zig Zag higlights trends by removing price changes smaller than \code{change} and interpolating
  lines between the extreme points.
}
\usage{
  ZigZag( HL, change=10, percent=TRUE, retrace=FALSE, lastExtreme=TRUE )
}
\arguments{
  \item{HL}{ Object that is coercible to xts or matrix and contains either a High-Low price
    series, or a Close price series. }
  \item{change}{ Minimum price movement, either in dollars or percent (see \code{percent}). }
  \item{percent}{ Use percentage or dollar change? }
  \item{retrace}{ Is \code{change} a retracement of the previous move, or an absolute change
    from peak to trough? }
  \item{lastExtreme}{ If the extreme price is the same over multiple periods, should the
    extreme price be the first or last observation? }
}
\details{
  The Zig Zag is non-predictive.  The purpose of the Zig Zag is filter noise and make chart
  patterns clearer.  It's more a visual tool than an indicator.
}
\value{
  A object of the same class as \code{HL} or a vector (if \code{try.xts}
  fails) containing the Zig Zag indicator.
}
\author{ Joshua Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/default.htm?url=ZigZag.htm}\cr
  \url{http://www.linnsoft.com/tour/techind/zigzag.htm}\cr
  \url{http://www.linnsoft.com/tour/techind/zigosc.htm}\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=127}\cr
  \url{http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:zigzag}\cr
}
\note{
  If High-Low prices are given, the function calculates the max/min using the high/low prices.
  Otherwise the function calculates the max/min of the single series.
}
%\seealso{
%  See \code{\link{CCI}}, \code{\link{ADX}}, \code{\link{TDI}}, and \code{\link{VHF}}
%  for other indicators that measure trend direction/strength.
%}
\examples{
  ## Get Data and Indicator ##
  data(ttrc)
  zz <- ZigZag( ttrc[,c("High", "Low")], change=20 )
}
\keyword{ ts }
