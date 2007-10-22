"CCI" <-
function(HLC, ma = list("SMA", n=20), c=0.015) {

  # Commodity Channel Index

  # http://www.fmlabs.com/reference/CCI.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=42
  # http://www.linnsoft.com/tour/techind/cci.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_CCI.html

  if(NCOL(HLC)==1) {
    message("Using Close/univariate price series."); flush.console()
    HLC <- as.vector(HLC)
  } else

  if(NCOL(HLC)==3) {
    message("Using typical price series."); flush.console()
    HLC <- rowMeans(HLC)
  } else

  stop("Price series must be either High-Low-Close, or Close/univariate.")

  mavg  <- do.call( ma[[1]], c( list(HLC), ma[-1] ) )
  mean.dev <- rep(NA, NROW(HLC))

  for(i in ma$n:NROW(HLC)) {
    mean.dev[i] <- sum( abs( mavg[i] - HLC[(i-ma$n+1):i] ) ) / ma$n
  }

  cci <- ( HLC - mavg ) / ( c * mean.dev )

  return( cci )
}
