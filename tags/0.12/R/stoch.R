"stoch" <-
function(HLC, n.fastK=14, ma.fastD=list("SMA", n=3), ma.slowD=ma.fastD) {

  # Stochastics

  # http://www.fmlabs.com/reference/StochasticOscillator.htm
  # http://www.equis.com/Customer/Resources/TAAZ?c=3&p=106
  # http://linnsoft.com/tour/techind/stoc.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_stochasticOscillator.html

  HLC <- as.matrix(HLC)

  # Calculation if HLC series is given
  if(ncol(HLC)==3) {
    message("Using High-Low-Close series")
    high  <- HLC[,1]
    low   <- HLC[,2]
    close <- HLC[,3]
  } else

  # Calculation if price vector is given
  if(ncol(HLC)==1) {
    message("Using Close series")
    high  <- HLC
    low   <- HLC
    close <- HLC
  } else

  stop("Price series must be either High-Low-Close, or Close")

  hmax <- roll.fn(high, n.fastK, FUN="max")
  lmin <- roll.fn( low, n.fastK, FUN="min")

  fastK <- (close - lmin) / (hmax - lmin)
  fastD <- do.call( ma.fastD[[1]], c( list(fastK), ma.fastD[-1] ) )
  slowD <- do.call( ma.slowD[[1]], c( list(fastD), ma.slowD[-1] ) )

  return( cbind( fastK, fastD, slowD ) )
}

