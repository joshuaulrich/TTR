#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"ATR" <-
function(HLC, ma=list("EMA", n=14, wilder=TRUE)) {

  # Average True Range / True Range

  # http://www.fmlabs.com/reference/TR.htm
  # http://www.fmlabs.com/reference/ATR.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=35
  # http://www.linnsoft.com/tour/techind/trueRange.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_ATR.html

  HLC <- as.matrix(HLC)
  closeLag <- c( HLC[1,3], HLC[-nrow(HLC),3] )

  trueHigh <- pmax( HLC[,1], closeLag )
  trueLow  <- pmin( HLC[,2], closeLag )
  tr        <- trueHigh - trueLow

  atr <- do.call( ma[[1]], c( list(tr), ma[-1] ) )

  return( cbind( tr, atr, trueHigh, trueLow ) )
}
