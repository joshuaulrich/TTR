#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2007-2008  Joshua M. Ulrich
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
    r <- ROC( OHLC[,4], 1, ... )
    rBar <- runSum( r, n-1 ) / (n-1)
    s <- sqrt( N/(n-2) * runSum( (r-rBar)^2 , n-1 ) )
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
    if(is.null(dots$k)) {
      k <- 0.34 / ( 1 + (n+1)/(n-1) )
    }

    s2o  <- N/(n-1) * runSum( log(OHLC[,1]/Cl1) -
                1/n * runSum( log(OHLC[,1]/Cl1),n) ) ^ 2
    s2c  <- N/(n-1) * runSum( log(OHLC[,4]/OHLC[,1]) -
                1/n * runSum( log(OHLC[,4]/OHLC[,1]),n) ) ^ 2
    s2rs <- volatility(OHLC=OHLC, n=n, calc="rogers.satchell", N=N, ...)
    s <- s2o + k*s2c + (1-k)*s2rs
  }

  reclass(s,OHLC)
}
