\name{ATR}
\alias{ATR}
\alias{TR}
\title{ True Range / Average True Range }
\concept{volatility}
\description{
  True range (TR) is a measure of volatility of a High-Low-Close series; average true range (ATR)
  is a Welles Wilder's style moving average of the TR.  Developed by J. Welles Wilder in 1978.
}
\usage{
  ATR(HLC, ma = list("EMA", n=14, wilder=TRUE))
}
\arguments{
  \item{HLC}{ Object able to be coerced to a matrix, which contains High-Low-Close prices. }
  \item{ma}{ A _list_ whose first component is a string containing the moving average function
             name; additional parameters may also be specified as _named_ components. }
}
\details{
  TR incorporates yesterday's close in the calculation (high minus low).  E.g. if
  yesterday's close was higher than today's high, then the TR would equal yesterday's
  close minus today's low.

  The ATR is a component of the Welles Wilder Directional Movement Index (\code{DX}, \code{ADX}).
}
\value{
  A matrix containing the columns:
  \item{ tr }{ The true range of the series. }
  \item{ atr }{ The average (as specified by \code{ma}) true range of the series. }
  \item{ true.high }{ The true high of the series. }
  \item{ true.low }{ The true low of the series. }
}
\author{ Josh Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/TR.htm}\cr
  \url{http://www.fmlabs.com/reference/ATR.htm}\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=35}\cr
  \url{http://www.linnsoft.com/tour/techind/trueRange.htm}\cr
  \url{http://stockcharts.com/education/IndicatorAnalysis/indic_ATR.html}\cr
}
\seealso{
  See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average options; and note
  Warning section.  See \code{\link{DX}}, which uses true range. 
  See \code{\link{chaikin.volatility}} for another volatility measure. }
\examples{
  data(nasdaq)
  atr <- ATR(nasdaq[,c("High","Low","Close")], ma = list("EMA", n=14, wilder=TRUE))
}
\keyword{ ts }