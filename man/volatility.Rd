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
  \item{OHLC}{ Object that is coercible to xts or matrix and contains
    Open-High-Low-Close prices (or only Close prices, if \code{calc="close"}). }
  \item{n}{ Number of periods for the volatility estimate. }
  \item{calc}{ The calculation (type) of estimator to use. }
  \item{N}{ Number of periods per year. }
  \item{\dots}{ Arguments to be passed to/from other methods. }
}
\details{
  \itemize{
    \item Close-to-Close Volatility (\code{calc="close"})\cr
    \deqn{ r_i = \ln \left(\frac{C_{i+1}}{C_i}\right) }
    \deqn{ \bar{r} = \frac{r_1+r_2+\ldots r_{n-1}}{n-1} }
    \deqn{ \sigma = \sqrt{\frac{Z}{n-2} \sum_{i=1}^{n-1}(r_i-\bar{r})^2} }

    \item OHLC Volatility: Garman and Klass (\code{calc="garman.klass"})\cr
    The Garman and Klass estimator for estimating historical volatility assumes
    Brownian motion with zero drift and no opening jumps (i.e. the opening =
    close of the previous period). This estimator is 7.4 times more efficient
    than the close-to-close estimator.
    \deqn{ \sigma = \sqrt{ \frac{Z}{n} \sum
      \left[ \textstyle\frac{1}{2}\displaystyle
        \left( \ln \frac{H_i}{L_i} \right)^2  - (2\ln 2-1)
        \left( \ln \frac{C_i}{O_i} \right)^2 \right] } }

    \item High-Low Volatility: Parkinson (\code{calc="parkinson"})\cr
    The Parkinson formula for estimating the historical volatility of an
    underlying based on high and low prices.
    \deqn{ \sigma = \sqrt{ \frac{Z}{n 4 \ln 2} \sum_{i=1}^{n}
      \left(\ln \frac{H_i}{L_i}\right)^2} }

    \item OHLC Volatility: Rogers and Satchell (\code{calc="rogers.satchell"})\cr
    The Roger and Satchell historical volatility estimator allows for non-zero
    drift, but assumed no opening jump.
    \deqn{ \sigma = \sqrt{ \textstyle\frac{Z}{n} \sum \left[
      \ln \textstyle\frac{H_i}{C_i} \ln \textstyle\frac{H_i}{O_i} +
      \ln \textstyle\frac{L_i}{C_i} \ln \textstyle\frac{L_i}{O_i} \right] } }

    \item OHLC Volatility: Garman and Klass - Yang and Zhang (\code{calc="gk.yz"})\cr
    This estimator is a modified version of the Garman and Klass estimator
    that allows for opening gaps.
    \deqn{ \sigma = \sqrt{ \textstyle\frac{Z}{n} \sum \left[
      \left( \ln \textstyle\frac{O_i}{C_{i-1}} \right)^2  +
        \textstyle\frac{1}{2}\displaystyle
      \left( \ln \textstyle\frac{H_i}{L_i} \right)^2 - (2\ln 2-1)
      \left( \ln \textstyle\frac{C_i}{O_i} \right)^2 \right] } }


    \item OHLC Volatility: Yang and Zhang (\code{calc="yang.zhang"})\cr
    The Yang and Zhang historical volatility estimator has minimum
    estimation error, and is independent of drift and opening gaps.  It can
    be interpreted as a weighted average of the Rogers and Satchell estimator,
    the close-open volatility, and the open-close volatility.\cr
    Users may override the default values of \eqn{\alpha} (1.34 by default) or
    \eqn{k} used in the calculation by specifying \code{alpha} or \code{k} in
    \code{\dots}, respectively. Specifying \code{k} will cause \code{alpha}  
    to be ignored, if both are provided.
    \deqn{ \sigma^2 = \sigma_o^2 + k\sigma_c^2 + (1-k)\sigma_{rs}^2 }
    \deqn{ \sigma_o^2 =\textstyle \frac{Z}{n-1} \sum
      \left( \ln\frac{O_i}{C_{i-1}}-\mu_o \right)^2 }
    \deqn{ \mu_o=\textstyle \frac{1}{n} \sum \ln\frac{O_i}{C_{i-1}} }
    \deqn{ \sigma_c^2 =\textstyle \frac{Z}{n-1} \sum
      \left( \ln\frac{C_i}{O_i}-\mu_c \right)^2 }
    \deqn{ \mu_c=\textstyle \frac{1}{n} \sum \ln\frac{C_i}{O_i} }
    \deqn{ \sigma_{rs}^2 = \textstyle\frac{Z}{n} \sum \left( 
      \ln \textstyle\frac{H_i}{C_i} \ln \textstyle\frac{H_i}{O_i} + 
      \ln \textstyle\frac{L_i}{C_i} \ln \textstyle\frac{L_i}{O_i} 
      \right) }
    \deqn{ k=\frac{\alpha}{1+\frac{n+1}{n-1}} }
  }
}
\value{
  A object of the same class as \code{OHLC} or a vector (if \code{try.xts}
  fails) containing the chosen volatility estimator values.
}
\author{ Joshua Ulrich }
\references{
  The following site(s) were used to code/document these indicators:\cr\cr
  Close-to-Close Volatility (\code{calc="close"}):\cr
  OHLC Volatility: Garman Klass (\code{calc="garman.klass"}):\cr
  \url{http://www.sitmo.com/eq/402}\cr
  High-Low Volatility: Parkinson (\code{calc="parkinson"}):\cr
  \url{http://www.sitmo.com/eq/173}\cr
  OHLC Volatility: Rogers Satchell (\code{calc="rogers.satchell"}):\cr
  \url{http://www.sitmo.com/eq/414}\cr
  OHLC Volatility: Garman Klass - Yang Zhang (\code{calc="gk.yz"}):\cr
  \url{http://www.sitmo.com/eq/409}\cr
  OHLC Volatility: Yang Zhang (\code{calc="yang.zhang"}):\cr
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
