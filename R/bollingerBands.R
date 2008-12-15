#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"BBands" <-
function(HLC, n=20, maType, sd=2, ...) {

  # Bollinger Bands

  # http://www.fmlabs.com/reference/Bollinger.htm
  # http://www.fmlabs.com/reference/BollingerWidth.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=36
  # http://www.linnsoft.com/tour/techind/bb.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_Bbands.html
  # http://stockcharts.com/education/IndicatorAnalysis/indic_BBWidth.htm

  HLC <- try.xts(HLC, error=FALSE)

  if(NCOL(HLC)==3) {
    if(is.xts(HLC)) {
      HLC <- xts(rowMeans(HLC),index(HLC))
    } else {
      HLC <- rowMeans(HLC)
    }
  } else
  if(NCOL(HLC)!=1) {
    stop("Price series must be either High-Low-Close, or Close/univariate.")
  }

  maArgs <- list(n=n, ...)
  # Default MA
  if(missing(maType)) {
    maType <- 'SMA'
  }

  mavg  <- do.call( maType, c( list(HLC), maArgs ) )

  # Calculate standard deviation by hand to incorporate various MAs
  sdev   <- runSD(HLC, n)

  up     <- mavg + sd * sdev
  dn     <- mavg - sd * sdev
  pctB  <- (HLC - dn) / (up - dn)

  res <- cbind(dn, mavg, up, pctB)
  reclass(res, HLC)
}
