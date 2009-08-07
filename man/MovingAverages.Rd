\name{MovingAverages}
\alias{MovingAverages}
\alias{SMA}
\alias{EMA}
\alias{WMA}
\alias{DEMA}
\alias{EVWMA}
\alias{ZLEMA}
\alias{MA}
\title{ Moving Averages }
\description{
  Calculate various moving averages (MA) of a series.
}
\usage{
  SMA(x, n=10)
  EMA(x, n=10, wilder=FALSE, ratio=NULL)
  WMA(x, n=10, wts=1:n)
 DEMA(x, n=10)
EVWMA(price, volume, n=10)
ZLEMA(x, n=10, ratio=NULL)
}
\arguments{
  \item{x}{ Price, volume, etc. series that is coercible to xts or matrix. }
  \item{price}{ Price series that is coercible to xts or matrix. }
  \item{volume}{ Volume series that is coercible to xts or matrix, that 
    corresponds to price series, or a constant.  See Notes. }
  \item{n}{ Number of periods to average over. }
  \item{wts}{ Vector of weights.  Length of \code{wts} vector must equal the
    length of \code{x}, or \code{n} (the default). }
  \item{wilder}{ logical; if \code{TRUE}, a Welles Wilder type EMA will be
    calculated; see notes. }
  \item{ratio}{ A smoothing/decay ratio to use (overrides \code{wilder} in EMA) }
}
\details{
  \code{SMA} calculates the arithmetic mean of the series over the past \code{n} observations.

  \code{EMA} calculates an exponentially-weighted mean, giving more weight to recent observations.
  See Warning section below.

  \code{WMA} is similar to an EMA, but with linear weighting if the length of \code{wts} is equal to
  \code{n}.  If the length of \code{wts} is equal to the length of \code{x}, the WMA will
  use the values of \code{wts} as weights.

  \code{DEMA} is calculated as: \code{DEMA = 2 * EMA(x,n) - EMA(EMA(x,n),n)}.

  \code{EVWMA} uses volume to define the period of the MA.

  \code{ZLEMA} is similar to an EMA, as it gives more weight to recent observations, but attempts to
  remove lag by subtracting data prior to \code{(n-1)/2} periods (default) to minimize
  the cumulative effect.
}
\value{
  A object of the same class as \code{x} or \code{price} or a vector
  (if \code{try.xts} fails) containing the columns:
  \item{SMA}{ Simple moving average. }
  \item{EMA}{ Exponential moving average. }
  \item{WMA}{ Weighted moving average. }
  \item{DEMA}{ Double-exponential moving average. }
  \item{EVWMA}{ Elastic, volume-weighted moving average. }
  \item{ZLEMA}{ Zero lag exponential moving average. }
}
\author{ Joshua Ulrich }
\references{
  The following site(s) were used to code/document this indicator:\cr
  \url{http://www.fmlabs.com/reference/ExpMA.htm}\cr
  \url{http://www.fmlabs.com/reference/WeightedMA.htm}\cr
  \url{http://www.fmlabs.com/reference/DEMA.htm}\cr
  \url{http://linnsoft.com/tour/techind/evwma.htm}\cr
  \url{http://www.fmlabs.com/reference/ZeroLagExpMA.htm}\cr
}
\note{ 
  For \code{EMA}, \code{wilder=FALSE} (the default) uses an exponential smoothing ratio of 
  \code{2/(n+1)}, while \code{wilder=TRUE} uses Welles Wilder's exponential smoothing ratio of
  \code{1/n}.

  Since \code{WMA} can accept a weight vector of length equal to the length of \code{x} or of
  length \code{n}, it can be used as a regular weighted moving average (in the case 
  \code{wts=1:n}) or as a moving average weighted by volume, another indicator, etc.

  For \code{EVWMA}, if \code{volume} is a series, \code{n} should be chosen so the sum of the
  volume for \code{n} periods approximates the total number of outstanding shares for the
  security being averaged.  If \code{volume} is a constant, it should represent the total
  number of outstanding shares for the security being averaged.
}
\section{Warning }{
  Some indicators (e.g. EMA, DEMA, EVWMA, etc.) are calculated using the indicators' own
  previous values, and are therefore unstable in the short-term.  As the indicator receives
  more data, its output becomes more stable.  See example below.
}
\seealso{
  See \code{\link{wilderSum}}, which is used in calculating a Welles Wilder type MA.
}
\examples{
  data(ttrc)
    ema.20 <-   EMA(ttrc[,"Close"], 20)
    sma.20 <-   SMA(ttrc[,"Close"], 20)
   dema.20 <-  DEMA(ttrc[,"Close"], 20)
  evwma.20 <- EVWMA(ttrc[,"Close"], ttrc[,"Volume"], 20)
  zlema.20 <- ZLEMA(ttrc[,"Close"], 20)

  ## Example of short-term instability of EMA
  ## (and other indicators mentioned above)
  x <- rnorm(100)
  tail( EMA(x[90:100],10), 1 )
  tail( EMA(x[70:100],10), 1 )
  tail( EMA(x[50:100],10), 1 )
  tail( EMA(x[30:100],10), 1 )
  tail( EMA(x[10:100],10), 1 )
  tail( EMA(x[ 1:100],10), 1 )
}
\keyword{ ts }
