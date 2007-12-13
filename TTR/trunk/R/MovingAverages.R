"SMA" <-
function(x, n=10) {

  # Simple Moving Average

  # http://www.fmlabs.com/reference/SimpleMA.htm
  # http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=74
  # http://linnsoft.com/tour/techind/movAvg.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_movingAvg.html

  x   <- as.vector(x)
  sma <- rollFUN(x, n, FUN="sum") / n

  return( sma )
}


"EMA" <-
function(x, n=10, wilder=FALSE, ratio=NULL) {

  # Exponential Moving Average

  # http://www.fmlabs.com/reference/ExpMA.htm
  # http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=74
  # http://linnsoft.com/tour/techind/movAvg.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_movingAvg.html

  x   <- as.vector(x)
  
  # Count NAs and ensure they only appear at beginning of data
  NAs <- sum( is.na(x) )
  if( NAs > 0 ) {
    if( any( !is.na(x[1:NAs]) ) ) stop("Series contains non-leading NAs")
  }
  x   <- na.omit(x)

  ema <- rep(1, NROW(x))

  if(is.null(ratio)) {
    if(wilder) ratio <- 1/n
    else       ratio <- 2/(n+1)
  }

  ema[n] <- mean(x[1:n])

  ema <- .Fortran( "ema", n = as.integer(n),
                          ma = as.double(ema),
                          x = as.double(x),
                          ratio = as.double(ratio),
                          lenma = as.integer(NROW(ema)),
                          lenx = as.integer(NROW(x)),
                          PACKAGE = "TTR" )$ma

  # replace 1:(n-1) with NAs and prepend NAs from original data
  ema[1:(n-1)] <- NA
  ema <- c( rep( NA, NAs ), ema ) 

  return( ema )
}


"WMA" <-
function(x, n=10, wts=1:n) {

  # Weighted Moving Average

  # http://www.fmlabs.com/reference/WeightedMA.htm
  # http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=74
  # http://linnsoft.com/tour/techind/movAvg.htm

  x   <- as.vector(x)

  if(NROW(wts)==n) {
    wma <- rollFUN(x, n, FUN="weighted.mean", w=wts)
  } else

  if(NROW(wts)==NROW(x)) {
    wma <- rollFUN(x*wts, n, FUN="sum") / rollFUN(wts, n, FUN="sum")
  } else

  stop("Length of 'wts' vector must equal length of 'x', or 'n'.")

  return( wma )
}


"DEMA" <-
function(x, n=10) {

  # Double Exponential Moving Average

  # http://www.fmlabs.com/reference/DEMA.htm

  x <- as.vector(x)
  dema <- 2 * EMA(x,n) - EMA(EMA(x,n),n)

  return( dema )
}


"EVWMA" <-
function(price, volume, n=10) {

  # Elastic, Volume-Weighted Moving Average

  # http://linnsoft.com/tour/techind/evwma.htm

  evwma <- rep(NA, NROW(price))
  evwma[n-1] <- price[n-1]

  if(NROW(volume)==1) {
    v.sum <- rep(volume, NROW(price))
  } else
    v.sum <- rollFUN(volume, n, FUN="sum")

  for(i in n:NROW(price)) {
    j <- i-n+1
    evwma[i] <- ( (v.sum[i]-volume[i])*evwma[i-1] + volume[i]*price[i] ) / v.sum[i]
  }
  return( evwma )

}


"ZLEMA" <-
function(x, n=10) {

  # Zero-Lag Exponential Moving Average

  # http://www.fmlabs.com/reference/ZeroLagExpMA.htm
  # http://linnsoft.com/tour/techind/movAvg.htm

  x     <- as.vector(x)
  zlema <- rep(NA, NROW(x))

  ratio <- 2/(n-1)
  lag   <- (n-1)/2

  zlema[n] <- mean(x[1:n])

  for(i in (n+1):NROW(x)) {
    zlema[i] <- ratio * ( 2 * x[i] - x[i-lag] ) + ( 1 - ratio ) * zlema[i-1]
  }

  return( zlema )
}
