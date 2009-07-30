\name{ADX}
\alias{ADX}
\alias{DI}
\alias{DX}
\title{ Welles Wilder's Directional Movement Index }
\concept{DMI}
\description{
  Directional Movement Index; developed by J. Welles Wilder.
}
\usage{
  ADX(HLC, n=14, maType, \dots)
}
\arguments{
  \item{HLC}{ Object that is coercible to xts or matrix and contains High-Low-Close prices. }
  \item{n}{ Number of periods to use for DX calculation (not ADX calculation). }
  \item{maType}{ A function or a string naming the function to be called. }
  \item{\dots}{ Other arguments to be passed to the \code{maType} function. }
}
\details{
  The \code{DIp}/\code{DIn} (positive/negative) is the percentage of the true range that is up/down.
}
\value{
  A object of the same class as \code{HLC} or a matrix (if \code{try.xts}
  fails) containing the columns:
  \item{ DIp }{ The positive Direction Index. }
  \item{ DIn }{ The negative Direction Index. }
  \item{ DX }{ The Direction Index. }
  \item{ ADX }{ The Average Direction Index (trend strength). }
}
\author{ Joshua Ulrich }
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
  See \code{\link{aroon}}, \code{\link{CCI}}, \code{\link{TDI}},
  \code{\link{VHF}}, \code{\link{GMMA}}
  for other indicators that measure trend direction/strength.
}
\examples{
  data(ttrc)
  dmi.adx <- ADX(ttrc[,c("High","Low","Close")])
}
\keyword{ ts }
