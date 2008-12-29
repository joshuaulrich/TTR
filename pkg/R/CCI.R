#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"CCI" <-
function(HLC, n=20, maType, c=0.015, ...) {

  # Commodity Channel Index

  # http://www.fmlabs.com/reference/CCI.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=42
  # http://www.linnsoft.com/tour/techind/cci.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_CCI.html

  HLC <- try.xts(HLC, error=FALSE)

  if(NCOL(HLC)==3) {
    if(is.xts(HLC)) {
      xa <- xcoredata(HLC)
      HLC <- xts(rowMeans(HLC),index(HLC))
      xcoredata(HLC) <- xa
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
  meanDev <- runMAD( HLC, n, center=mavg, stat="mean" )

  cci <- ( HLC - mavg ) / ( c * meanDev )

  reclass(cci, HLC)
}
