\name{volatility}
\alias{volatility}
\alias{garman.klass}
\alias{parkinson}
\alias{rogers.satchell}
\alias{gk.yz}
\alias{yang.zhang}
\title{ Volatility }
\description{
  Selected volatility estimators/indicators; various authors.
}
\usage{
  volatility(OHLC, n=10, calc="close", N=260, \dots)
}
\arguments{
  \item{OHLC}{ Object that is coercible to xts or matrix and contains Open-High-Low-Close prices. }
  \item{n}{ Number of periods for the volatility estimate. }
  \item{calc}{ The calculation (type) of estimator to use. }
  \item{N}{ Number of periods per year. }
  \item{\dots}{ Arguments to be passed to/from other methods. }
}
\details{
  Close-to-Close Volatility (\code{close}):\cr
  Historical volatility calculation using close-to-close prices.\cr\cr

  OHLC Volatility: Garman and Klass (\code{garman.klass}):\cr
  The Garman and Klass estimator for estimating historical volatility assumes
  Brownian motion with zero drift and no opening jumps (i.e. the opening = close
  of the previous period). This estimator is 7.4 times more efficient than the
  close-to-close estimator.\cr\cr

  High-Low Volatility: Parkinson (\code{parkinson}):\cr
  The Parkinson formula for estimating the historical volatility of an
  underlying based on high and low prices.\cr\cr

  OHLC Volatility: Rogers and Satchell (\code{rogers.satchell}):\cr
  The Roger and Satchell historical volatility estimator allows for non-zero
  drift, but assumed no opening jump.\cr\cr
  
  OHLC Volatility: Garman and Klass - Yang and Zhang (\code{gk.yz}):\cr
  This estimator is a modified version of the Garman and Klass estimator
  that allows for opening gaps.\cr\cr
  
  OHLC Volatility: Yang and Zhang (\code{yang.zhang}):\cr
  The Yang and Zhang historical volatility estimator has minimum
  estimation error, and is independent of drift and opening gaps.  It can
  be interpreted as a weighted average of the Rogers and Satchell estimator,
  the close-open volatility, and the open-close volatility.
}
\value{
  A object of the same class as \code{OHLC} or a vector (if \code{try.xts}
  fails) containing the chosen volatility estimator values.
}
\author{ Joshua Ulrich }
\references{
  The following site(s) were used to code/document these indicators:\cr\cr
  Close-to-Close Volatility (\code{close}):\cr
  \url{http://www.sitmo.com/eq/172}\cr
  OHLC Volatility: Garman Klass (\code{garman.klass}):\cr
  \url{http://www.sitmo.com/eq/402}\cr
  High-Low Volatility: Parkinson (\code{parkinson}):\cr
  \url{http://www.sitmo.com/eq/173}\cr
  OHLC Volatility: Rogers Satchell (\code{rogers.satchell}):\cr
  \url{http://www.sitmo.com/eq/414}\cr
  OHLC Volatility: Garman Klass - Yang Zhang (\code{gk.yz}):\cr
  \url{http://www.sitmo.com/eq/409}\cr
  OHLC Volatility: Yang Zhang (\code{yang.zhang}):\cr
  \url{http://www.sitmo.com/eq/417}\cr
}
%\note{
%}
\seealso{
  See \code{\link{TR}} and \code{\link{chaikinVolatility}} for
  other volatility measures.
}
\examples{
  data(ttrc)
  ohlc <- ttrc[,c("Open","High","Low","Close")]
  vClose <- volatility(ohlc, calc="close")
  vGK <- volatility(ohlc, calc="garman")
  vParkinson <- volatility(ohlc, calc="parkinson")
  vRS <- volatility(ohlc, calc="rogers")
}
\keyword{ ts }
