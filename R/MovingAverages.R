#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2007-2010  Joshua M. Ulrich
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

"SMA" <-
function(x, n=10) {

  # Simple Moving Average

  # http://www.fmlabs.com/reference/SimpleMA.htm
  # http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=74
  # http://linnsoft.com/tour/techind/movAvg.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_movingAvg.html

  ma <- runMean( x, n )

  return(ma)
}

#-------------------------------------------------------------------------#

"EMA" <-
function (x, n=10, wilder=FALSE, ratio=NULL) {

  # Exponential Moving Average

  # http://www.fmlabs.com/reference/ExpMA.htm
  # http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=74
  # http://linnsoft.com/tour/techind/movAvg.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_movingAvg.html

  x <- try.xts(x, error=as.matrix)
  
  # Check for non-leading NAs
  # Leading NAs are handled in the C code
  x.na <- xts:::naCheck(x, n)

  # Determine decay ratio
  if(is.null(ratio)) {
    if(wilder) ratio <- 1/n
    else       ratio <- 2/(n+1)
  }

  # Call C routine
  ma <- .Call("ema", x, n, ratio, PACKAGE = "TTR")

  reclass(ma, x)
}

#-------------------------------------------------------------------------#

"DEMA" <-
function(x, n=10) {

  # Double Exponential Moving Average

  # http://www.fmlabs.com/reference/DEMA.htm

  dema <- 2 * EMA(x,n) - EMA(EMA(x,n),n)

  return( dema )
}

#-------------------------------------------------------------------------#

"WMA" <-
function(x, n=10, wts=1:n) {

  # Weighted Moving Average

  # http://www.fmlabs.com/reference/WeightedMA.htm
  # http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=74
  # http://linnsoft.com/tour/techind/movAvg.htm

  x <- try.xts(x, error=as.matrix)
  wts <- try.xts(wts, error=as.matrix)

  if( !any( NROW(wts) == c( NROW(x), n ) ) )
    stop("Length of 'wts' must equal the length of 'x' or 'n'")
  if( n < 1 || n > NROW(x) )
    stop("Invalid 'n'")

  # Count NAs, ensure they're only at beginning of data, then remove.
  NAx <- sum( is.na(x) )
  NAw <- sum( is.na(wts) )
  NAs <- max( NAx, NAw )
  if( NAs > 0 ) {
    if( any( is.na(  x[-(1:NAx)]) ) )
      stop("'x' contains non-leading NAs")
    if( any( is.na(wts[-(1:NAw)]) ) )
      stop("'wts' contains non-leading NAs")
  }
  
  if( NROW(wts) == n ) {
    
    x <- na.omit(x)
    NAs <- NAx

    if( any(is.na(wts)) )
      stop("'wts' vector of length 'n' cannot have NA values")

    # Call Fortran routine
    ma <- .Fortran( "wma", ia = as.double(x),
                           lia = as.integer(NROW(x)),
                           wts = as.double(wts),
                           n = as.integer(n),
                           oa = as.double(x),
                           loa = as.integer(NROW(x)),
                           PACKAGE = "TTR",
                           DUP = TRUE )$oa
   
  } else {
    
    xw <- na.omit( cbind(x, wts) )
    ma <- runSum( xw[,1]*xw[,2], n) / runSum(xw[,2], n)
  }

  # replace 1:(n-1) with NAs and prepend NAs from original data
  ma[1:(n-1)] <- NA
  ma <- c( rep( NA, NAs ), ma )

  reclass(ma, x)
}

#-------------------------------------------------------------------------#

"EVWMA" <-
function(price, volume, n=10) {

  # Elastic, Volume-Weighted Moving Average

  # http://linnsoft.com/tour/techind/evwma.htm

  price <- try.xts(price, error=as.matrix)
  volume <- try.xts(volume, error=as.matrix)

  if( !any( NROW(volume) == c( NROW(price), 1 ) ) )
    stop("Length of 'volume' must equal 1 or the length of 'price'")
  if( n < 1 || n > NROW(price) )
    stop("Invalid 'n'")

  pv <- cbind(price, volume)

  # Check for non-leading NAs
  # Leading NAs are handled in the C code
  pv.na <- xts:::naCheck(pv, n)

  # Call C routine
  ma <- .Call("evwma", pv[,1], pv[,2], n, PACKAGE = "TTR")

  # Convert back to original class
  reclass(ma, price)
}

#-------------------------------------------------------------------------#

"ZLEMA" <-
function (x, n=10, ratio=NULL) {

  # Zero-Lag Exponential Moving Average

  # http://www.fmlabs.com/reference/ZeroLagExpMA.htm
  # http://linnsoft.com/tour/techind/movAvg.htm

  x <- try.xts(x, error=as.matrix)
  
  # Count NAs, ensure they're only at beginning of data, then remove.
  NAs <- sum( is.na(x) )
  if( NAs > 0 ) {
    if( any( is.na(x[-(1:NAs)]) ) )
      stop("Series contains non-leading NAs")
  }
  x   <- na.omit(x)

  # Initialize ma vector
  ma <- rep(1, NROW(x))
  ma[n] <- mean(x[1:n])

  # Determine decay ratio
  if(is.null(ratio)) {
    ratio <- 2/(n-1)
  }

  # Call Fortran routine
  ma <- .Fortran( "zlema", ia = as.double(x),
                           lia = as.integer(NROW(x)),
                           n = as.integer(n),
                           oa = as.double(ma),
                           loa = as.integer(NROW(ma)),
                           ratio = as.double(ratio),
                           PACKAGE = "TTR",
                           DUP = TRUE )$oa

  # replace 1:(n-1) with NAs and prepend NAs from original data
  ma[1:(n-1)] <- NA
  ma <- c( rep( NA, NAs ), ma ) 
  
  reclass(ma, x)
}

#-------------------------------------------------------------------------#

"VWAP" <- "VWMA" <-
function(price, volume, n=10) {

  # Volume-weighted average price
  # Volume-weighted moving average

  res <- WMA(price, n=n, volume)
  return(res)

}

