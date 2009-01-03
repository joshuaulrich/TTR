"OBV" <-
function(price, volume) {

  # On Balance Volume

  # http://www.fmlabs.com/reference/OBV.htm
  # http://www.equis.com/Customer/Resources/TAAZ?c=3&p=82
  # http://linnsoft.com/tour/techind/obVol.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic-obv.htm

  price <- as.vector(price)
  obv   <- cumsum( ifelse( c( 1, ROC(price, n=1)[-1] ) > 0, volume, -volume ) )

  return( obv )
}
