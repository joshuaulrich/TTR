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

  x <- try.xts(x, error=FALSE)
  
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
                         PACKAGE = "TTR" )$oa

  # Replace 1:(n-1) with NAs and prepend NAs from original data
  is.na(ma) <- c(1:(n-1))
  ma <- c( rep( NA, x.na$NAs ), ma ) 
  
  # Convert back to original class
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
                           PACKAGE = "TTR" )$oa
   
  } else {
    
    xw <- na.omit( cbind(x, wts) )
    ma <- runSum( xw[,1]*xw[,2], n) / runSum(xw[,2], n)
  }

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
