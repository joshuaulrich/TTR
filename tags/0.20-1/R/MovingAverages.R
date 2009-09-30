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
  
  # Count NAs, ensure they're only at beginning of data, then remove.
  # Avoid na.omit() because it will cause problems for reclass()
  x.na <- naCheck(x, n)

  # Initialize ma vector
  ma <- rep(1, NROW(x))
  ma[x.na$beg] <- mean(x[x.na$nonNA[1]:x.na$beg])

  # Determine decay ratio
  if(is.null(ratio)) {
    if(wilder) ratio <- 1/n
    else       ratio <- 2/(n+1)
  }

  # Call Fortran routine
  ma <- .Fortran( "ema", ia = as.double(x[x.na$nonNA]),
                         lia = as.integer(NROW(x.na$nonNA)),
                         n = as.integer(n),
                         oa = as.double(ma[x.na$nonNA]),
                         loa = as.integer(NROW(x.na$nonNA)),
                         ratio = as.double(ratio),
                         PACKAGE = "TTR",
                         DUP = FALSE )$oa

  # Replace 1:(n-1) with NAs and prepend NAs from original data
  is.na(ma) <- c(1:(n-1))
  ma <- c( rep( NA, x.na$NAs ), ma ) 
  
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

  # Count NAs, ensure they're only at beginning of data, then remove.
  NAp <- sum( is.na(price) )
  NAv <- sum( is.na(volume) )
  NAs <- max( NAp, NAv )
  if( NAs > 0 ) {
    if( any( is.na( price[-(1:NAp)]) ) )
      stop("'price' contains non-leading NAs")
    if( any( is.na(volume[-(1:NAv)]) ) )
      stop("'volume' contains non-leading NAs")
  }
  pv <- na.omit( cbind(price, volume) )

  # Initialize ma vector 
  ma <- rep(0, NROW(pv))
  ma[n] <- pv[n,1]

  if(NROW(volume)==1) {
    vSum <- rep(volume, NROW(pv))
  } else {
    vSum <- runSum(pv[,2], n)
    vSum[1:(n-1)] <- pv[1:(n-1),2]
  }

  # Call Fortran routine
  ma <- .Fortran( "evwma", ip = as.double(pv[,1]),
                           iv = as.double(pv[,2]),
                           ivs = as.double(vSum),
                           lia = as.integer(NROW(pv)),
                           n = as.integer(n),
                           oa = as.double(ma),
                           loa = as.integer(NROW(ma)),
                           PACKAGE = "TTR",
                           DUP = FALSE )$oa

  # replace 1:(n-1) with NAs and prepend NAs from original data
  ma[1:(n-1)] <- NA
  ma <- c( rep( NA, max( NAp, NAv ) ), ma )

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
