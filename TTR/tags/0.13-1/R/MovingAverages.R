"SMA" <-
function(x, n=10) {

  # Simple Moving Average

  # http://www.fmlabs.com/reference/SimpleMA.htm
  # http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=74
  # http://linnsoft.com/tour/techind/movAvg.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_movingAvg.html

  # There needs to be options to make the first 'n' obs
  # either NA, 0, or something else for all MA functions.

  x   <- as.vector(x)
  sma <- rollFUN(x, n, FUN="sum") / n

  return( sma )
}

# -----------------------------------------------------------

"EMA" <-
function(x, n=10, wilder=FALSE) {

  # Exponential Moving Average

  # http://www.fmlabs.com/reference/ExpMA.htm
  # http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=74
  # http://linnsoft.com/tour/techind/movAvg.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_movingAvg.html

  x   <- as.vector(x)
  ema <- as.vector(x)

  if(wilder) ratio <- 1/n
  else       ratio <- 2/(n+1)

  ema[n] <- mean(x[1:n])

  for(i in (n+1):NROW(x)) {
    ema[i] <- x[i] * ratio + ema[i-1] * (1-ratio)
  }
  return( ema )
}

# -----------------------------------------------------------

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

# -----------------------------------------------------------

"DEMA" <-
function(x, n=10) {

  # Double Exponential Moving Average

  # http://www.fmlabs.com/reference/DEMA.htm

  x <- as.vector(x)
  dema <- 2 * EMA(x,n) - EMA(EMA(x,n),n)

  return( dema )
}

# -----------------------------------------------------------

"EVWMA" <-
function(price, volume, n=10) {

  # Elastic, Volume-Weighted Moving Average

  # http://linnsoft.com/tour/techind/evwma.htm

  evwma <- vector("numeric", NROW(price))

  if(NROW(volume)==1) {
    v.sum <- rep(volume, NROW(price))
  } else
    v.sum <- rollFUN(volume, n, FUN="sum")

  for(i in 1:(NROW(price)-n+1)) {
    j <- i+n-1
    evwma[j] <- ( (v.sum[j]-volume[j])*evwma[j-1] + volume[j]*price[j] ) / v.sum[j]
  }
  return( evwma )
}

# -----------------------------------------------------------

"ZLEMA" <-
function(x, n=10) {

  # Zero-Lag Exponential Moving Average

  # http://www.fmlabs.com/reference/ZeroLagExpMA.htm
  # http://linnsoft.com/tour/techind/movAvg.htm

  x     <- as.vector(x)
  zlema <- as.vector(x)

  ratio <- 2/(n-1)
  lag   <- (n-1)/2

  zlema[n] <- mean(x[1:n])

  for(i in (n+1):NROW(x)) {
    zlema[i] <- ratio * ( 2 * x[i] - x[i-lag] ) + ( 1 - ratio ) * zlema[i-1]
  }

  return( zlema )
}
