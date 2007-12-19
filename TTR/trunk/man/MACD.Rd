\name{oscillator}
\alias{oscillator}
\alias{MACD}
\title{ Oscillators }
\description{
  The oscillators documented in this page compare a fast moving average (MA) of
  a series with a slow MA of the same series.  The most popular price oscillator is
  probably the MACD, which was developed by Gerald Appel.
}
\usage{
  MACD(x, ma.fast=list("EMA", n=12), ma.slow=list("EMA", n=26),
             ma.sig=list("EMA", n=9), percent=FALSE)
}
\arguments{
  \item{x}{ Series to use; usually price, but can be volume, etc. }
  \item{ma.fast}{ A \emph{list} whose first component is a string containing the fast moving average
    function name; additional parameters may also be specified as \emph{named} components. }
  \item{ma.slow}{ Similar to \code{ma.fast}, but for the fast moving average. }
  \item{ma.sig}{ Similar to \code{ma.fast}, but for the signal line moving average. }
  \item{percent}{ logical; if \code{TRUE}, the percentage difference between the fast and slow moving
    averages is returned, otherwise the difference between the respective averages is returned. }
}
\details{
  The MACD function either subtracts the fast MA from the slow MA, or finds the rate
  of change between the fast MA and the slow MA.
}
\value{
  The MACD function returns a matrix containing the columns:
  \item{ macd }{ The price (volume, etc.) oscillator. }
  \item{ signal }{ The oscillator signal line (a moving average of the oscillator). }
}
\author{ Josh Ulrich }
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

  macd  <- MACD( ttrc[,"Close"], ma.fast=list("EMA", n=12),
                                 ma.slow=list("EMA", n=26),
                                 ma.sig =list("EMA", n= 9) )

}
\keyword{ ts }
