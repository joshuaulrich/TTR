"TRIX" <-
function(price, ma1=list("EMA", n=20), ma2=ma1, ma3=ma1,
              ma.sig=list("EMA", n=9), percent=TRUE) {

  # Triple Smoothed Exponential Oscillator

  # http://www.fmlabs.com/reference/default.htm?url=TRIX.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=114
  # http://www.linnsoft.com/tour/techind/trix.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_trix.htm

  price  <- as.matrix(price)
  mavg1 <- do.call( ma1[[1]], c( list(price), ma1[-1] ) )
  mavg2 <- do.call( ma2[[1]], c( list(mavg1), ma2[-1] ) )
  mavg3 <- do.call( ma3[[1]], c( list(mavg2), ma3[-1] ) )

  if(percent) {
    TRIX <- 100 * ROC(mavg3, n=1, na=NA, type="discrete")
  } else {
    TRIX <- momentum( mavg3, n=1, na=NA )
  }

  signal <- do.call( ma.sig[[1]], c( list(TRIX), ma.sig[-1] ) )

  return( cbind( TRIX, signal ) )

}
