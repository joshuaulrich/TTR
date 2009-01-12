\name{volatility}
\alias{volatility}
\alias{garman.klass}
\alias{parkinson}
\alias{rogers.satchell}
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

  OHLC Volatility: Garman Klass (\code{garman.klass}):\cr
  The Garman and Klass estimator for estimating historical volatility assumes
  Brownian motion with zero drift and no opening jumps (i.e. the opening = close
  of the previous period). This estimator is 7.4 times more efficient than the
  close-to-close estimator.\cr\cr

  High-Low Volatility: Parkinson (\code{parkinson}):\cr
  The Parkinson formula for estimating the historical volatility of an
  underlying based on high and low prices.\cr\cr

  OHLC Volatility: Rogers Satchell (\code{rogers.satchell}):\cr
  The Roger and Satchell historical volatility estimator allows for non-zero
  drift, but assumed no opening jump.
}
\value{
  A vector containing the chosen volatility estimator values.
}
\author{ Josh Ulrich }
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
}
%\note{
%}
\seealso{
  See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average options; and note
  Warning section.  See \code{\link{TR}} for another volatility measure.
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
