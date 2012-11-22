#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2007-2012  Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#



#'Volatility
#'
#'Selected volatility estimators/indicators; various authors.
#'
#'\itemize{ \item Close-to-Close Volatility (\code{calc="close"})\cr \deqn{ r_i
#'= \ln \left(\frac{C_{i+1}}{C_i}\right) } \deqn{ \bar{r} =
#'\frac{r_1+r_2+\ldots r_{n-1}}{n-1} } \deqn{ \sigma = \sqrt{\frac{Z}{n-2}
#'\sum_{i=1}^{n-1}(r_i-\bar{r})^2} }
#'
#'\item OHLC Volatility: Garman and Klass (\code{calc="garman.klass"})\cr The
#'Garman and Klass estimator for estimating historical volatility assumes
#'Brownian motion with zero drift and no opening jumps (i.e. the opening =
#'close of the previous period). This estimator is 7.4 times more efficient
#'than the close-to-close estimator.  \deqn{ \sigma = \sqrt{ \frac{Z}{n} \sum
#'\left[ \textstyle\frac{1}{2}\displaystyle \left( \ln \frac{H_i}{L_i}
#'\right)^2 - (2\ln 2-1) \left( \ln \frac{C_i}{O_i} \right)^2 \right] } }
#'
#'\item High-Low Volatility: Parkinson (\code{calc="parkinson"})\cr The
#'Parkinson formula for estimating the historical volatility of an underlying
#'based on high and low prices.  \deqn{ \sigma = \sqrt{ \frac{Z}{n 4 \ln 2}
#'\sum_{i=1}^{n} \left(\ln \frac{H_i}{L_i}\right)^2} }
#'
#'\item OHLC Volatility: Rogers and Satchell (\code{calc="rogers.satchell"})\cr
#'The Roger and Satchell historical volatility estimator allows for non-zero
#'drift, but assumed no opening jump.  \deqn{ \sigma = \sqrt{
#'\textstyle\frac{Z}{n} \sum \left[ \ln \textstyle\frac{H_i}{C_i} \ln
#'\textstyle\frac{H_i}{O_i} + \ln \textstyle\frac{L_i}{C_i} \ln
#'\textstyle\frac{L_i}{O_i} \right] } }
#'
#'\item OHLC Volatility: Garman and Klass - Yang and Zhang
#'(\code{calc="gk.yz"})\cr This estimator is a modified version of the Garman
#'and Klass estimator that allows for opening gaps.  \deqn{ \sigma = \sqrt{
#'\textstyle\frac{Z}{n} \sum \left[ \left( \ln \textstyle\frac{O_i}{C_{i-1}}
#'\right)^2 + \textstyle\frac{1}{2}\displaystyle \left( \ln
#'\textstyle\frac{H_i}{L_i} \right)^2 - (2\ln 2-1) \left( \ln
#'\textstyle\frac{C_i}{O_i} \right)^2 \right] } }
#'
#'\item OHLC Volatility: Yang and Zhang (\code{calc="yang.zhang"})\cr The Yang
#'and Zhang historical volatility estimator has minimum estimation error, and
#'is independent of drift and opening gaps.  It can be interpreted as a
#'weighted average of the Rogers and Satchell estimator, the close-open
#'volatility, and the open-close volatility.\cr Users may override the default
#'values of \eqn{\alpha} (1.34 by default) or \eqn{k} used in the calculation
#'by specifying \code{alpha} or \code{k} in \code{\dots{}}, respectively.
#'Specifying \code{k} will cause \code{alpha} to be ignored, if both are
#'provided.  \deqn{ \sigma^2 = \sigma_o^2 + k\sigma_c^2 + (1-k)\sigma_{rs}^2 }
#'\deqn{ \sigma_o^2 =\textstyle \frac{Z}{n-1} \sum \left(
#'\ln\frac{O_i}{C_{i-1}}-\mu_o \right)^2 } \deqn{ \mu_o=\textstyle \frac{1}{n}
#'\sum \ln\frac{O_i}{C_{i-1}} } \deqn{ \sigma_c^2 =\textstyle \frac{Z}{n-1}
#'\sum \left( \ln\frac{C_i}{O_i}-\mu_c \right)^2 } \deqn{ \mu_c=\textstyle
#'\frac{1}{n} \sum \ln\frac{C_i}{O_i} } \deqn{ \sigma_{rs}^2 =
#'\textstyle\frac{Z}{n} \sum \left( \ln \textstyle\frac{H_i}{C_i} \ln
#'\textstyle\frac{H_i}{O_i} + \ln \textstyle\frac{L_i}{C_i} \ln
#'\textstyle\frac{L_i}{O_i} \right) } \deqn{ k=\frac{\alpha}{1+\frac{n+1}{n-1}}
#'} }
#'
#'@aliases volatility garman.klass parkinson rogers.satchell gk.yz yang.zhang
#'@param OHLC Object that is coercible to xts or matrix and contains
#'Open-High-Low-Close prices (or only Close prices, if \code{calc="close"}).
#'@param n Number of periods for the volatility estimate.
#'@param calc The calculation (type) of estimator to use.
#'@param N Number of periods per year.
#'@param \dots Arguments to be passed to/from other methods.
#'@return A object of the same class as \code{OHLC} or a vector (if
#'\code{try.xts} fails) containing the chosen volatility estimator values.
#'@author Joshua Ulrich
#'@seealso See \code{\link{TR}} and \code{\link{chaikinVolatility}} for other
#'volatility measures.
#'@references The following site(s) were used to code/document these
#'indicators:\cr\cr Close-to-Close Volatility (\code{calc="close"}):\cr OHLC
#'Volatility: Garman Klass (\code{calc="garman.klass"}):\cr
#'\url{http://www.sitmo.com/eq/402}\cr High-Low Volatility: Parkinson
#'(\code{calc="parkinson"}):\cr \url{http://www.sitmo.com/eq/173}\cr OHLC
#'Volatility: Rogers Satchell (\code{calc="rogers.satchell"}):\cr
#'\url{http://www.sitmo.com/eq/414}\cr OHLC Volatility: Garman Klass - Yang
#'Zhang (\code{calc="gk.yz"}):\cr \url{http://www.sitmo.com/eq/409}\cr OHLC
#'Volatility: Yang Zhang (\code{calc="yang.zhang"}):\cr
#'\url{http://www.sitmo.com/eq/417}\cr
#'@keywords ts
#'@examples
#'
#'  data(ttrc)
#'  ohlc <- ttrc[,c("Open","High","Low","Close")]
#'  vClose <- volatility(ohlc, calc="close")
#'  vGK <- volatility(ohlc, calc="garman")
#'  vParkinson <- volatility(ohlc, calc="parkinson")
#'  vRS <- volatility(ohlc, calc="rogers")
#'
"volatility" <-
function(OHLC, n=10, calc="close", N=260, ...) {

  OHLC <- try.xts(OHLC, error=as.matrix)

  # Choose an arg name that doesn't clash with ROC's 'type' arg
  calc <- match.arg(calc,
            c("close","garman.klass","parkinson",
              "rogers.satchell","gk.yz","yang.zhang"))
  
  # s       Volatility
  # N       Number of closing prices in a year
  # n       Number of historical prices used for the volatility estimate
  # ci      The closing price on the ith day
  # ri      Log return on the ith day

  # Historical Close-to-Close Volatility
  # http://www.sitmo.com/eq/172
  if( calc=="close" ) {
    # Add univariate case from Cedrick Johnson's R-SIG-Finance post
    if( NCOL(OHLC) == 1 ) {
      r <- ROC(OHLC[, 1], 1, ...)
    } else {
      r <- ROC(OHLC[, 4], 1, ...)
    }
    s <- sqrt(N) * runSD(r , n-1)
  }

  # Historical Open-High-Low-Close Volatility: Garman Klass
  # http://www.sitmo.com/eq/402
  if( calc=="garman.klass" ) {
    s <- sqrt( N/n * runSum( .5 * log(OHLC[,2]/OHLC[,3])^2 -
               (2*log(2)-1) * log(OHLC[,4]/OHLC[,1])^2 , n ) )
  }

  if( calc=="parkinson" ) {
    # Historical High-Low Volatility: Parkinson
    # http://www.sitmo.com/eq/173
    s <- sqrt( N/(4*n*log(2)) * runSum( log(OHLC[,2]/OHLC[,3])^2, n ) )
  }

  if( calc=="rogers.satchell" ) {
    # Historical Open-High-Low-Close Volatility: Rogers Satchell
    # http://www.sitmo.com/eq/414
    s <- sqrt( N/n * runSum(
               log(OHLC[,2]/OHLC[,4]) * log(OHLC[,2]/OHLC[,1]) +
               log(OHLC[,3]/OHLC[,4]) * log(OHLC[,3]/OHLC[,1]), n ) )
  }

  if( calc=="gk.yz" ) {
  #if( calc=="garman.klass.yang.zhang" ) {
    # Historical Open-High-Low-Close Volatility: Garman and Klass (Yang Zhang)
    # http://www.sitmo.com/eq/409
    if(is.xts(OHLC)) {
      Cl1 <- lag(OHLC[,4])
    } else {
      Cl1 <- c( NA, OHLC[-NROW(OHLC),4] )
    }
    s <- sqrt( N/n * runSum(
               log(OHLC[,1]/Cl1)^2 +
               .5 * log(OHLC[,2]/OHLC[,3])^2 -
               (2*log(2)-1) * log(OHLC[,4]/OHLC[,1])^2 , n) )

    #s <- sqrt( Z/n * runSum(
    #          log(op/cl[-1])^2 +
    #          .5*log(hi/lo)^2 -
    #          (2*log(2)-1)*log(cl/op)^2 ) )
  }

  if( calc=="yang.zhang" ) {
    # Historical Open-High-Low-Close Volatility: Yang Zhang
    # http://www.sitmo.com/eq/417
    if(is.xts(OHLC)) {
      Cl1 <- lag(OHLC[,4])
    } else {
      Cl1 <- c( NA, OHLC[-NROW(OHLC),4] )
    }
    
    dots <- list(...)
    if(is.null(dots$alpha)) {
      alpha <- 1.34
    }
    if(is.null(dots$k)) {
      k <- (alpha-1) / ( alpha + (n+1)/(n-1) )
    }

    s2o <- N * runVar(log(OHLC[,1] / Cl1), n=n)
    s2c <- N * runVar(log(OHLC[,4] / OHLC[,1]), n=n)
    s2rs <- volatility(OHLC=OHLC, n=n, calc="rogers.satchell", N=N, ...)
    s <- sqrt(s2o + k*s2c + (1-k)*(s2rs^2))
  }

  reclass(s,OHLC)
}
