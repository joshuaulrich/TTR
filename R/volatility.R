#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2007-2013  Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 2 of the License, or
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
#'\itemize{ 
#'\item Close-to-Close Volatility (\code{calc="close"})\cr
#'\deqn{ \sigma_{cl} = \sqrt{\frac{Z}{n-2} \sum_{i=1}^{n-1}(r_i-\bar{r})^2}
#'}{sqrt(N) * runSD(ROC(Cl), n-1)}
#'\deqn{where\;\; r_i = \log \left(\frac{C_i}{C_{i-1}}\right) }{}
#'\deqn{and\;\; \bar{r} = \frac{r_1+r_2+\ldots +r_{n-1}}{n-1} }{}
#'
#'\item OHLC Volatility: Garman and Klass (\code{calc="garman.klass"})\cr The
#'Garman and Klass estimator for estimating historical volatility assumes
#'Brownian motion with zero drift and no opening jumps (i.e. the opening =
#'close of the previous period). This estimator is 7.4 times more efficient
#'than the close-to-close estimator.\cr
#'\deqn{ \sigma = \sqrt{ \frac{Z}{n} \sum
#'  \left[ \textstyle\frac{1}{2}\displaystyle
#'    \left( \log \frac{H_i}{L_i} \right)^2  - (2\log 2-1)
#'    \left( \log \frac{C_i}{O_i} \right)^2 \right] }
#'}{sqrt(N/n * runSum(0.5 * log(Hi/Lo)^2 -
#'           (2*log(2)-1) * log(Cl/Op)^2, n))}
#'
#'\item High-Low Volatility: Parkinson (\code{calc="parkinson"})\cr 
#'The Parkinson formula for estimating the historical volatility of 
#'an underlying based on high and low prices.\cr
#'\deqn{ \sigma = \sqrt{ \frac{Z}{4 n \times \log 2} \sum_{i=1}^{n}
#'  \left(\log \frac{H_i}{L_i}\right)^2}
#'}{sqrt(N/(4*n*log(2)) * runSum(log(Hi/Lo)^2, n))}
#'
#'\item OHLC Volatility: Rogers and Satchell (\code{calc="rogers.satchell"})\cr
#'The Roger and Satchell historical volatility estimator allows for non-zero
#'drift, but assumed no opening jump.\cr
#'\deqn{ \sigma = \sqrt{ \textstyle\frac{Z}{n} \sum \left[
#'  \log \textstyle\frac{H_i}{C_i} \times \log \textstyle\frac{H_i}{O_i} +
#'  \log \textstyle\frac{L_i}{C_i} \times \log \textstyle\frac{L_i}{O_i} \right] }
#'}{sqrt(N/n * runSum(log(Hi/Cl) * log(Hi/Op) +
#'                    log(Lo/Cl) * log(Lo/Op), n))}
#'
#'\item OHLC Volatility: Garman and Klass - Yang and Zhang
#'(\code{calc="gk.yz"})\cr This estimator is a modified version of the Garman
#'and Klass estimator that allows for opening gaps.\cr
#'\deqn{ \sigma = \sqrt{ \textstyle\frac{Z}{n} \sum \left[
#'  \left( \log \textstyle\frac{O_i}{C_{i-1}} \right)^2  +
#'    \textstyle\frac{1}{2}\displaystyle
#'  \left( \log \textstyle\frac{H_i}{L_i} \right)^2 - (2 \times \log 2-1)
#'  \left( \log \textstyle\frac{C_i}{O_i} \right)^2 \right] }
#'}{sqrt(N/n * runSum(log(Op/lag(Cl,1))^2 +
#'  0.5 * log(Hi/Lo)^2 - (2*log(2)-1) * log(Cl/Op)^2 , n))}
#'
#'\item OHLC Volatility: Yang and Zhang (\code{calc="yang.zhang"})\cr The Yang
#'and Zhang historical volatility estimator has minimum estimation error, and
#'is independent of drift and opening gaps.  It can be interpreted as a
#'weighted average of the Rogers and Satchell estimator, the close-open
#'volatility, and the open-close volatility.
#'
#'Users may override the default values of \eqn{\alpha} (1.34 by default) or
#'\eqn{k} used in the calculation by specifying \code{alpha} or \code{k} in
#'\code{\dots}, respectively. Specifying \code{k} will cause \code{alpha} to be
#'ignored, if both are provided.\cr
#'\deqn{ \sigma^2 = \sigma_o^2 + k\sigma_c^2 + (1-k)\sigma_{rs}^2
#'}{ s <- sqrt(s2o + k*s2c + (1-k)*(s2rs^2)) }
#'\deqn{ \sigma_o^2 =\textstyle \frac{Z}{n-1} \sum
#'  \left( \log \frac{O_i}{C_{i-1}}-\mu_o \right)^2
#'}{ s2o <- N * runVar(log(Op/lag(Cl,1)), n=n) }
#'\deqn{ \mu_o=\textstyle \frac{1}{n} \sum \log \frac{O_i}{C_{i-1}} }{}
#'\deqn{ \sigma_c^2 =\textstyle \frac{Z}{n-1} \sum
#'  \left( \log \frac{C_i}{O_i}-\mu_c \right)^2
#'}{ s2c <- N * runVar(log(Cl/Op), n=n) }
#'\deqn{ \mu_c=\textstyle \frac{1}{n} \sum \log \frac{C_i}{O_i} }{}
#'\deqn{ \sigma_{rs}^2 = \textstyle\frac{Z}{n} \sum \left( 
#'  \log \textstyle\frac{H_i}{C_i} \times \log \textstyle\frac{H_i}{O_i} + 
#'  \log \textstyle\frac{L_i}{C_i} \times \log \textstyle\frac{L_i}{O_i} 
#'  \right)
#'}{ s2rs <- volatility(OHLC, n, "rogers.satchell", N, ...) }
#'\deqn{ k=\frac{\alpha}{1+\frac{n+1}{n-1}}
#'}{ k <- (alpha-1) / (alpha + (n+1)/(n-1)) }
#'}
#'
#'@aliases volatility garman.klass parkinson rogers.satchell gk.yz yang.zhang
#'@param OHLC Object that is coercible to xts or matrix and contains
#'Open-High-Low-Close prices (or only Close prices, if \code{calc="close"}).
#'@param n Number of periods for the volatility estimate.
#'@param calc The calculation (type) of estimator to use.
#'@param N Number of periods per year.
#'@param mean0 Use a mean of 0 rather than the sample mean.
#'@param \dots Arguments to be passed to/from other methods.
#'@return A object of the same class as \code{OHLC} or a vector (if
#'\code{try.xts} fails) containing the chosen volatility estimator values.
#'@author Joshua Ulrich
#'@seealso See \code{\link{TR}} and \code{\link{chaikinVolatility}} for other
#'volatility measures.
#'@references The following sites were used to code/document these
#'indicators. All were created by Thijs van den Berg under the GNU Free
#'Documentation License and were retrieved on 2008-04-20. The original
#'links are dead, but can be accessed via internet archives.\cr
#'\cr Close-to-Close Volatility (\code{calc="close"}):\cr
#'\url{https://web.archive.org/web/20100421083157/http://www.sitmo.com/eq/172}\cr
#'\cr OHLC Volatility: Garman Klass (\code{calc="garman.klass"}):\cr
#'\url{https://web.archive.org/web/20100326172550/http://www.sitmo.com/eq/402}\cr
#'\cr High-Low Volatility: Parkinson (\code{calc="parkinson"}):\cr
#'\url{https://web.archive.org/web/20100328195855/http://www.sitmo.com/eq/173}\cr
#'\cr OHLC Volatility: Rogers Satchell (\code{calc="rogers.satchell"}):\cr
#'\url{https://web.archive.org/web/20091002233833/http://www.sitmo.com/eq/414}\cr
#'\cr OHLC Volatility: Garman Klass - Yang Zhang (\code{calc="gk.yz"}):\cr
#'\url{https://web.archive.org/web/20100326215050/http://www.sitmo.com/eq/409}\cr
#'\cr OHLC Volatility: Yang Zhang (\code{calc="yang.zhang"}):\cr
#'\url{https://web.archive.org/web/20100326215050/http://www.sitmo.com/eq/409}\cr
#'@keywords ts
#'@examples
#'
#' data(ttrc)
#' ohlc <- ttrc[,c("Open","High","Low","Close")]
#' vClose <- volatility(ohlc, calc="close")
#' vClose0 <- volatility(ohlc, calc="close", mean0=TRUE)
#' vGK <- volatility(ohlc, calc="garman")
#' vParkinson <- volatility(ohlc, calc="parkinson")
#' vRS <- volatility(ohlc, calc="rogers")
#'
#'@export
"volatility" <-
function(OHLC, n=10, calc="close", N=260, mean0=FALSE, ...) {

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
    if( isTRUE(mean0) ) {
      # This is an alternative SD calculation using an effective mean of 0
      s <- sqrt(N) * sqrt(runSum(r^2, n-1) / (n-2))
    } else {
      # This is the standard SD calculation using the sample mean
      s <- sqrt(N) * runSD(r, n-1)
    }
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
      Cl1 <- lag.xts(OHLC[,4])
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
      Cl1 <- lag.xts(OHLC[,4])
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
