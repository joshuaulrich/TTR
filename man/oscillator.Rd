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
  oscillator(x, ma.fast=list("EMA", n=10), ma.slow=list("EMA", n=20),
             ma.sig=list("EMA", n=10), percent=FALSE)
        MACD(x)
}
\arguments{
  \item{x}{ Series to use; usually price, but can be volume, etc. }
  \item{ma.fast}{ Similar to \code{ma.slow}, but for the fast moving average. }
  \item{ma.slow}{ A \emph{list} whose first component is a string containing the slow moving average
    function name; additional parameters may also be specified as \emph{named} components. }
  \item{ma.sig}{ Similar to \code{ma.slow}, but for the signal line moving average. }
  \item{percent}{ logical; if \code{TRUE}, the percentage difference between the slow and fast moving
    averages is returned, otherwise the difference between the respective averages is returned. }
}
\details{
  The oscillator function either subtracts the short MA from the long MA, or finds the rate
  of change between the short MA and the long MA.
}
\value{
  The MACD and oscillator functions return a matrix containing the columns:
  \item{ oscillator }{ The price (volume, etc.) oscillator. }
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
  The MACD is a special case of the general oscillator applied to price.  The MACD function
  is provided for convenience. Time periods for the MACD are often given as 26 and 12, but
  the function originally used exponential constants of 0.075 and 0.15, which are closer
  to 25.6667 and 12.3333 periods.
}
\seealso{
  See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average options; and note Warning
  section.
}
\examples{
  data(ttrc)

  macd <- MACD(ttrc[,"Close"])
  osc  <- oscillator( ttrc[,"Close"], ma.fast=list("EMA", n=12),
                      ma.slow=list("EMA", n=26), ma.sig=list("EMA", n=9) )

  # Should be TRUE
  identical(macd,osc)
}
\keyword{ ts }
