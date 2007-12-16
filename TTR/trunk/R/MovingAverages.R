#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"SMA" <-
function(x, n=10) {

  # Simple Moving Average

  # http://www.fmlabs.com/reference/SimpleMA.htm
  # http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=74
  # http://linnsoft.com/tour/techind/movAvg.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_movingAvg.html

  # Count NAs, ensure they're only at beginning of data, then remove.
  NAs <- sum( is.na(x) )
  if( NAs > 0 ) {
    if( any( is.na(x[-(1:NAs)]) ) )
      stop("Series contains non-leading NAs")
  }
  x   <- na.omit(x)

  ma <- runSum( x, n ) / n

  # replace 1:(n-1) with NAs and prepend NAs from original data
  ma[1:(n-1)] <- NA
  ma <- c( rep( NA, NAs ), ma ) 
  
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

  x   <- as.vector(x)
  
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
    if(wilder) ratio <- 1/n
    else       ratio <- 2/(n+1)
  }

  # Call Fortran routine
  ma <- .Fortran( "ema", ia = as.double(x),
                             lia = as.integer(NROW(x)),
                             n = as.integer(n),
                             oa = as.double(ma),
                             loa = as.integer(NROW(ma)),
                             ratio = as.double(ratio),
                             PACKAGE = "TTR" )$oa

  # replace 1:(n-1) with NAs and prepend NAs from original data
  ma[1:(n-1)] <- NA
  ma <- c( rep( NA, NAs ), ma ) 
  
  return(ma)
}

#-------------------------------------------------------------------------#

"DEMA" <-
function(x, n=10) {

  # Double Exponential Moving Average

  # http://www.fmlabs.com/reference/DEMA.htm

  x <- as.vector(x)
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

  # Count NAs, ensure they're only at beginning of data, then remove.
  NAs <- sum( is.na(x) )
  if( NAs > 0 ) {
    if( any( is.na(x[-(1:NAs)]) ) )
      stop("Series contains non-leading NAs")
  }
  x   <- na.omit(x)

  # Initialize ma vector
  x   <- as.vector(x)

  if(NROW(wts)==n) {

    # Call Fortran routine
    ma <- .Fortran( "wma", ia = as.double(x),
                               lia = as.integer(NROW(x)),
                               wts = as.double(wts),
                               n = as.integer(n),
                               oa = as.double(x),
                               loa = as.integer(NROW(x)),
                               PACKAGE = "TTR" )$oa

   
  } else

  if(NROW(wts)==NROW(x)) {
     ma <- runSumProd(x, wts, n) / runSum(wts, n)
  } else
    stop("Length of 'wts' vector must equal length of 'x', or 'n'.")

  # replace 1:(n-1) with NAs and prepend NAs from original data
  ma[1:(n-1)] <- NA
  ma <- c( rep( NA, NAs ), ma )

  return( ma )
}

#-------------------------------------------------------------------------#

"EVWMA" <-
function(price, volume, n=10) {

  # Elastic, Volume-Weighted Moving Average

  # http://linnsoft.com/tour/techind/evwma.htm

  if( !any( NROW(volume) == c( NROW(price), 1 ) ) )
    stop("Length of 'volume' must equal 1 or the length of 'price'")
  if( n < 1 | n > NROW(price) )
    stop("Invalid 'n'")

  # Count NAs, ensure they're only at beginning of data, then remove.
  NAp <- sum( is.na(price) )
  NAv <- sum( is.na(volume) )
  if( max( NAp, NAv ) > 0 ) {
    if( any( is.na( price[-(1:NAp)]) ) )
      stop("'price' contains non-leading NAs")
    if( any( is.na(volume[-(1:NAv)]) ) )
      stop("'volume' contains non-leading NAs")
  }
  z   <- na.omit( cbind(price, volume) )

  # Initialize ma vector 
  ma <- rep(0, NROW(z))
  ma[n] <- z[n,1]

  if(NROW(volume)==1) {
    vSum <- rep(volume, NROW(z))
  } else {
    vSum <- runSum(z[,2], n)
    vSum[1:(n-1)] <- z[1:(n-1),2]
  }

  # Call Fortran routine
  ma <- .Fortran( "evwma", ip = as.double(z[,1]),
                               iv = as.double(z[,2]),
                               ivs = as.double(vSum),
                               lia = as.integer(NROW(z)),
                               n = as.integer(n),
                               oa = as.double(ma),
                               loa = as.integer(NROW(ma)),
                               PACKAGE = "TTR" )$oa

  # replace 1:(n-1) with NAs and prepend NAs from original data
  ma[1:(n-1)] <- NA
  ma <- c( rep( NA, max( NAp, NAv ) ), ma )

  return( ma )
}

#-------------------------------------------------------------------------#

"ZLEMA" <-
function (x, n=10, ratio=NULL) {

  # Zero-Lag Exponential Moving Average

  # http://www.fmlabs.com/reference/ZeroLagExpMA.htm
  # http://linnsoft.com/tour/techind/movAvg.htm

  x   <- as.vector(x)
  
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
                               PACKAGE = "TTR" )$oa

  # replace 1:(n-1) with NAs and prepend NAs from original data
  ma[1:(n-1)] <- NA
  ma <- c( rep( NA, NAs ), ma ) 
  
  return(ma)
}
