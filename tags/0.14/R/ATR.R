#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"ATR" <-
function(HLC, n=14, maType="EMA", wilder=TRUE, ...) {

  # Average True Range / True Range

  # http://www.fmlabs.com/reference/TR.htm
  # http://www.fmlabs.com/reference/ATR.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=35
  # http://www.linnsoft.com/tour/techind/trueRange.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_ATR.html

  HLC <- as.matrix(HLC)
  closeLag <- c( HLC[1,3], HLC[-NROW(HLC),3] )

  trueHigh <- pmax( HLC[,1], closeLag )
  trueLow  <- pmin( HLC[,2], closeLag )
  tr       <- trueHigh - trueLow

  # If necessary, combine 'wilder' formal default with `...' arg(s)
  if( missing(maType) && missing(wilder) ) {
    maArgs <- list(n=n, wilder=TRUE)
  } else
  if( !missing(wilder) ) {
    maArgs <- list(n=n, wilder=wilder, ...)
  } else
    maArgs <- list(n=n, ...)

  atr <- do.call( maType, c( list(tr), maArgs ) )

  return( cbind( tr, atr, trueHigh, trueLow ) )
}
