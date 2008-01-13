#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"CCI" <-
function(HLC, ma=list("SMA", n=20), c=0.015) {

  # Commodity Channel Index

  # http://www.fmlabs.com/reference/CCI.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=42
  # http://www.linnsoft.com/tour/techind/cci.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_CCI.html

  if(NCOL(HLC)==1) {
    HLC <- as.vector(HLC)
  } else

  if(NCOL(HLC)==3) {
    HLC <- rowMeans(HLC)
  } else

  stop("Price series must be either High-Low-Close, or Close/univariate.")

  mavg  <- do.call( ma[[1]], c( list(HLC), ma[-1] ) )
  meanDev <- runMAD( HLC, ma$n, center=mavg, stat="mean" )

  cci <- ( HLC - mavg ) / ( c * meanDev )

  return( cci )
}
