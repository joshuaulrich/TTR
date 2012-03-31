\name{aroon}
\alias{aroon}
\title{ Aroon }
\concept{trend}
\description{
  The Aroon indicator attempts to identify starting trends.  The indicator consists
  of up and down lines, which measure how long it has been since the highest high/lowest
  low has occurred in the last \code{n} periods.  Developed by Tushar Chande in 1995.
}
\usage{
  aroon(HL, n=20)
}
\arguments{
  \item{HL}{ Object that is coercible to xts or matrix and contains either a High-Low price
    series, or a Close price series. }
  \item{n}{ Number of periods to use in the calculation. }
}
\details{
  Aroon up (down) is the elapsed time, expressed as a percentage, between today and the highest
  (lowest) price in the last \code{n} periods.  If today's price is a new high (low)
  Aroon up (down) will be 100. Each subsequent period without another new high (low) causes
  Aroon up (down) to decrease by (1 / \code{n}) x 100.
}
\value{
  A object of the same class as \code{HL} or a matrix (if \code{try.xts}
  fails) containing the columns:
  \item{ aroonUp }{ The Aroon up indicator. }
  \item{ aroonDn }{ The Aroon down indicator. }
  \item{ oscillator }{ The Aroon oscillator (\code{aroonUp - aroonDn}). }
}
\author{ Joshua Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/Aroon.htm}\cr
  \url{http://www.fmlabs.com/reference/AroonOscillator.htm}\cr
  \url{http://www.linnsoft.com/tour/techind/aroon.htm}\cr
  \url{http://stockcharts.com/education/IndicatorAnalysis/indic-Aroon.htm}\cr
}
\note{
  If High-Low prices are given, the function calculates the max/min using the high/low prices.
  Otherwise the function calculates the max/min of the single series.

  Up (down) trends are indicated when the aroonUp(Dn) is between 70 and 100.  Strong trends
  are indicated when when the aroonUp(Dn) is above 70 while the aroonDn(Up) is below 30.
  Also, crossovers may be useful.
}
\seealso{
  See \code{\link{CCI}}, \code{\link{ADX}}, \code{\link{TDI}},
  \code{\link{VHF}}, \code{\link{GMMA}}
  for other indicators that measure trend direction/strength.
}
\examples{
  ## Get Data and Indicator ##
  data(ttrc)
  trend <- aroon( ttrc[,c("High", "Low")], n=20 )
}
\keyword{ ts }
