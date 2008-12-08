#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"ATR" <-
function(HLC, n=14, maType, ...) {

  # Average True Range / True Range

  # http://www.fmlabs.com/reference/TR.htm
  # http://www.fmlabs.com/reference/ATR.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=35
  # http://www.linnsoft.com/tour/techind/trueRange.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_ATR.html

  HLC <- try.xts(HLC, error=FALSE)
  
  if(is.xts(HLC)) {
    closeLag <- lag(HLC[,3])
  } else {
    closeLag <- c( NA, HLC[-NROW(HLC),3] )
  }

  trueHigh <- pmax( HLC[,1], closeLag, na.rm=FALSE )
  trueLow  <- pmin( HLC[,2], closeLag, na.rm=FALSE )
  tr       <- trueHigh - trueLow

  maArgs <- list(n=n, ...)
  
  # Default Welles Wilder EMA
  if(missing(maType)) {
    maType <- 'EMA'
    maArgs$wilder <- TRUE
  }

  atr <- do.call( maType, c( list(tr), maArgs ) )

  # Convert back to original class
  result <- cbind( tr, atr, trueHigh, trueLow )
  colnames(result) <- c('tr','atr','trueHigh','trueLow')
  reclass( result, HLC )
}
