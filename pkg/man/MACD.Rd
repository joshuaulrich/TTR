\name{MACD}
\alias{MACD}
\title{ MACD Oscillator }
\description{
  The MACD was developed by Gerald Appel and is probably the most popular price oscillator.
  The MACD function documented in this page compares a fast moving average (MA) of
  a series with a slow MA of the same series.  It can be used as a generic oscillator
  for any univariate series, not only price.
}
\usage{
  MACD(x, nFast=12, nSlow=26, nSig=9, maType, percent=TRUE, \dots)
}
\arguments{
  \item{x}{ Object that is coercible to xts or matrix; usually price,
    but can be volume, etc. }
  \item{nFast}{ Number of periods for fast moving average. }
  \item{nSlow}{ Number of periods for slow moving average. }
  \item{nSig}{ Number of periods for signal moving average. }
  \item{maType}{ Either: \cr (1) A function or a string naming the function to be called, or\cr
    (2) a \emph{list} with the first component like (1) above, and additional parameters
    specified as \emph{named} components.  See Examples. }
  \item{percent}{ logical; if \code{TRUE}, the percentage difference between the fast and slow moving
    averages is returned, otherwise the difference between the respective averages is returned. }
  \item{\dots}{ Other arguments to be passed to the \code{maType} function in case (1) above. }
}
\details{
  The MACD function either subtracts the fast MA from the slow MA, or finds the rate
  of change between the fast MA and the slow MA.
}
\value{
  A object of the same class as \code{x} or a matrix (if \code{try.xts}
  fails) containing the columns:
  \item{ macd }{ The price (volume, etc.) oscillator. }
  \item{ signal }{ The oscillator signal line (a moving average of the oscillator). }
}
\author{ Joshua Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/MACD.htm}\cr
  \url{http://www.fmlabs.com/reference/PriceOscillator.htm}\cr
  \url{http://www.fmlabs.com/reference/PriceOscillatorPct.htm}\cr
  \url{http://stockcharts.com/education/IndicatorAnalysis/indic_MACD1.html}\cr
  \url{http://stockcharts.com/education/IndicatorAnalysis/indic_priceOscillator.html}\cr
}
\note{
  The MACD is a special case of the general oscillator applied to price.  The MACD can be
  used as a general oscillator applied to any series. Time periods for the MACD are often
  given as 26 and 12, but the function originally used exponential constants of 0.075 and
  0.15, which are closer to 25.6667 and 12.3333 periods.
}
\seealso{
  See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average options; and note Warning
  section.
}
\examples{
  data(ttrc)

  macd  <- MACD( ttrc[,"Close"], 12, 26, 9, maType="EMA" )
  macd2 <- MACD( ttrc[,"Close"], 12, 26, 9,
           maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA)) )
}
\keyword{ ts }
