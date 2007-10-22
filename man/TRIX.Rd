\name{TRIX}
\alias{TRIX}
\title{ Triple Smoothed Exponential Oscillator }
\description{
  The TRIX indicator calculates the rate of change of a triple exponential moving average.
  Developed by Jack K. Hutson.
}
\usage{
  TRIX(price, ma1 = list("EMA", n=20), ma2 = ma1, ma3 = ma1,
              ma.sig = list("EMA", n=9), percent = TRUE)
}
\arguments{
  \item{price}{ Price series to use. }
  \item{ma1}{ A \emph{list} whose first component is a string containing the moving average
    function name; additional parameters may also be specified as \emph{named} components. }
  \item{ma2}{ See \code{ma1}. }
  \item{ma3}{ See \code{ma1}. }
  \item{ma.sig}{ Similar to \code{ma1}, but for the signal moving average. }
  \item{percent}{ logical; if \code{TRUE}, the rate of change is calculated using the \code{ROC}
    function, otherwise the the \code{momentum} function is used. }
}
\details{
  The TRIX is calculated as follows:\cr
  3MA = \code{ma3}( \code{ma2}( \code{ma1}(\code{price}) ) )\cr
  trix = 100 * [ 3MA(t) / 3MA(t-1) - 1 ]
}
\value{
  A vector containing the TRIX values.
}
\author{ Josh Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/default.htm?url=TRIX.htm}\cr
  \url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=114}\cr
  \url{http://www.linnsoft.com/tour/techind/trix.htm}\cr
  \url{http://stockcharts.com/education/IndicatorAnalysis/indic_trix.htm}\cr
}
\note{
  Buy/sell signals are generated when the TRIX crosses above/below zero.  A nine-period EMA
  of the TRIX is used as a default signal line.  Buy/sell signals are generated when the
  TRIX crosses above/below the signal line and is also above/below zero.
}
\seealso{
  See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average options; and note
  Warning section.
}
\examples{
  data(ttrc)
  trix <- TRIX( ttrc[,"Close"] )
}
\keyword{ ts }
