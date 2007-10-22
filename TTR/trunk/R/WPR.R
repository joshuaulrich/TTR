"WPR" <-
function(HLC, n=14) {

  # William's Percent R (similar to Stochastics' fast %K)

  # http://www.fmlabs.com/reference/WilliamsR.htm
  # http://www.equis.com/Customer/Resources/TAAZ?c=3&p=126
  # http://linnsoft.com/tour/techind/willR.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_williamsR.html

  HLC <- as.matrix(HLC)

  # Calculation if HLC series is given
  if(NCOL(HLC)==3) {
    message("Using High-Low-Close series"); flush.console()
    high  <- HLC[,1]
    low   <- HLC[,2]
    close <- HLC[,3]
  } else

  # Calculation if price vector is given
  if(NCOL(HLC)==1) {
    message("Using Close price series"); flush.console()
    high  <- HLC
    low   <- HLC
    close <- HLC
  } else

  stop("Price series must be either High-Low-Close, or Close")

  hmax <- rollFUN(high, n, FUN="max")
  lmin <- rollFUN( low, n, FUN="min")

  pct.R <- (hmax - close) / (hmax - lmin)

  return( pct.R )
}
