\name{TRIX}
\alias{TRIX}
\title{ Triple Smoothed Exponential Oscillator }
\description{
  The TRIX indicator calculates the rate of change of a triple exponential moving average.
  Developed by Jack K. Hutson.
}
\usage{
  TRIX(price, n=20, nSig=9, maType, percent=TRUE, \dots)
}
\arguments{
  \item{price}{ Price series that is coercible to xts or matrix. }
  \item{n}{ Number of periods for moving average. }
  \item{nSig}{ Number of periods for signal line moving average. }
  \item{maType}{ Either: \cr(1) A function or a string naming the function to be called, or\cr
    (2) a \emph{list} with the first component like (1) above, and additional parameters
    specified as \emph{named} components.  See Examples. }
  \item{percent}{ logical; if \code{TRUE}, the rate of change is calculated using the \code{ROC}
    function, otherwise the \code{momentum} function is used. }
  \item{\dots}{ Other arguments to be passed to the \code{maType} function in case (1) above. }
}
\details{
  The TRIX is calculated as follows:\cr
  3MA = \code{MA}( \code{MA}( \code{MA}(\code{price}) ) )\cr
  trix = 100 * [ 3MA(t) / 3MA(t-1) - 1 ]
}
\value{
  A object of the same class as \code{price} or a vector (if \code{try.xts}
  fails) containing the TRIX values.
}
\author{ Joshua Ulrich }
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
  trix  <- TRIX(ttrc[,"Close"])
  trix4 <- TRIX(ttrc[,"Close"],
    maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA), list(DEMA)))
}
\keyword{ ts }
