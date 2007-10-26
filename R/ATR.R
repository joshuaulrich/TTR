"ATR" <-
function(HLC, ma=list("EMA", n=14, wilder=TRUE)) {

  # Average True Range / True Range

  # http://www.fmlabs.com/reference/TR.htm
  # http://www.fmlabs.com/reference/ATR.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=35
  # http://www.linnsoft.com/tour/techind/trueRange.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_ATR.html

  HLC <- as.matrix(HLC)
  close.lag <- c( HLC[1,3], HLC[-nrow(HLC),3] )

  true.high <- pmax( HLC[,1], close.lag )
  true.low  <- pmin( HLC[,2], close.lag )
  tr        <- true.high - true.low

  atr <- do.call( ma[[1]], c( list(tr), ma[-1] ) )

  return( cbind( tr, atr, true.high, true.low ) )
}
