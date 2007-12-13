"bollingerBands" <-
function(HLC, ma=list("SMA", n=20), sd=list(FUN="sd", n=2)) {

  # Bollinger Bands

  # http://www.fmlabs.com/reference/Bollinger.htm
  # http://www.fmlabs.com/reference/BollingerWidth.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=36
  # http://www.linnsoft.com/tour/techind/bb.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_Bbands.html
  # http://stockcharts.com/education/IndicatorAnalysis/indic_BBWidth.htm

  if(NCOL(HLC)==1) {
    HLC <- as.vector(HLC)
  } else

  if(NCOL(HLC)==3) {
    HLC <- rowMeans(HLC)
  } else

  stop("Price series must be either High-Low-Close, or Close/univariate.")

  mavg  <- do.call( ma[[1]], c( list(HLC), ma[-1] ) )

  # Calculate standard deviation by hand to incorporate various MAs
  sdev   <- rollFUN(HLC, ma$n, FUN=sd$FUN)

  up     <- mavg + sd$n * sdev
  dn     <- mavg - sd$n * sdev
  pct.b  <- (HLC - dn) / (up - dn)

  return( cbind(dn, mavg, up, pct.b) )
}
