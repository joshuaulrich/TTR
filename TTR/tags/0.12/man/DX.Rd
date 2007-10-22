\name{DX}
\alias{DX}
\alias{DI}
\alias{ADX}
\title{ Welles Wilder's Directional Movement Index }
\concept{DMI}
\description{
  Directional Movement Index; developed by J. Welles Wilder.
}
\usage{
  DX(HLC, n=14, ma.adx=list("EMA", n=n, wilder=TRUE))
}
\arguments{
  \item{HLC}{ Object able to be coerced to a matrix, which contains High-Low-Close prices. }
  \item{n}{ Number of periods to use for DX calculation (not ADX calculation). }
  \item{ma.adx}{ A _list_ whose first component is a string containing the ADX moving average
    function name; additional parameters may also be specified as _named_ components. }
}
\details{
  The \code{DIp}/\code{DIn} (positive/negative) is the percentage of the true range that is up/down.
}
\value{
  A matrix containing the columns:
  \item{ DIp }{ The positive Direction Index. }
  \item{ DIn }{ The negative Direction Index. }
  \item{ DX }{ The Direction Index. }
  \item{ ADX }{ The Average Direction Index (trend strength). }
}
\author{ Josh Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/DI.htm}\cr
  \url{http://www.fmlabs.com/reference/DX.htm}\cr
  \url{http://www.fmlabs.com/reference/ADX.htm}\cr
  \url{http://www.fmlabs.com/reference/ADXR.htm}\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=49}\cr
  \url{http://linnsoft.com/tour/techind/dirInd.htm}\cr
  \url{http://linnsoft.com/tour/techind/adx.htm}\cr
  \url{http://linnsoft.com/tour/techind/adxr.htm}\cr
  \url{http://stockcharts.com/education/IndicatorAnalysis/indic_ADX.html}\cr
}
\note{
  A buy/sell signal is generated when the +/-DI crosses up over the -/+DI, when the DX/ADX
  signals a strong trend.  A high/low DX signals a strong/weak trend.  DX is usually smoothed
  with a moving average (i.e. the ADX).
}
\seealso{
  See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average options; and note
  Warning section.  The DX calculation uses \code{\link{ATR}}.
  See \code{\link{aroon}}, \code{\link{CCI}}, \code{\link{TDI}}, \code{\link{VHF}}
  for other indicators that measure trend direction/strength.
}
\examples{
  data(nasdaq)
  dmi.adx <- DX(nasdaq[,c("High","Low","Close")])
}
\keyword{ ts }