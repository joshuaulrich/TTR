#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"BBands" <-
function(HLC, n=20, maType="SMA", sd=2, ...) {

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

  maArgs <- list(n=n, ...)
  mavg  <- do.call( maType, c( list(HLC), maArgs ) )

  # Calculate standard deviation by hand to incorporate various MAs
  sdev   <- sqrt( runVar(HLC, n) )

  up     <- mavg + sd * sdev
  dn     <- mavg - sd * sdev
  pctB  <- (HLC - dn) / (up - dn)

  return( cbind(dn, mavg, up, pctB) )
}
