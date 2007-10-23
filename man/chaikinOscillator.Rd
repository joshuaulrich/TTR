\name{chaikinOscillator}
\alias{chaikinOscillator}
\title{ Chaikin Oscillator }
\description{
  The Chaikin Oscillator is an oscillator applied to the Chaikin Accumulation / Distribution line.
}
\usage{
  chaikinOscillator(HLC, volume, ma.slow = list("EMA", n=10),
                     ma.fast = list("EMA", n=3), percent=FALSE)
}
\arguments{
  \item{HLC}{ Object able to be coerced to a matrix, which contains High-Low-Close prices. }
  \item{volume}{ Vector or matrix of volume observations corresponding to the \code{HLC} object. }
  \item{ma.slow}{ A \emph{list} whose first component is a string containing the slow moving
    average function name; additional parameters may also be specified as \emph{named} components. }
  \item{ma.fast}{ A \emph{list} whose first component is a string containing the fast moving
    average function name; additional parameters may also be specified as \emph{named} components. }
  \item{percent}{ logical; if \code{TRUE}, the percentage difference between the slow and fast moving
    averages is returned, otherwise the difference between the respective averages is returned. }
}
\details{
  The Chaikin Oscillator is calculated similarly to other price oscillators: by subtracting a slow
  moving average of the A/D line from a fast moving average of the A/D line.
}
\value{
  A vector containing the Chaikin Oscillator values.
}
\author{ Josh Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/ChaikinOscillator.htm}\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=41}\cr
  \url{http://stockcharts.com/education/IndicatorAnalysis/indic_ChaikinOscillator.html}\cr
}
\note{ 
  When the Chaikin Oscillator crosses above (below) zero, it indicates a buy (sell) signal.
  Also look for price divergence to indicate bullish or bearish conditions.
}
\seealso{
  See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average options; and note Warning
  section.  Also see \code{\link{CLV}}, \code{\link{chaikinAD}}, and \code{\link{oscillator}}.
}
\examples{
  data(ttrc)
  oscillator <- chaikinOscillator(ttrc[,c("High","Low","Close")], ttrc[,"Volume"])
}
\keyword{ ts }
