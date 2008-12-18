#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"OBV" <-
function(price, volume) {

  # On Balance Volume

  # http://www.fmlabs.com/reference/OBV.htm
  # http://www.equis.com/Customer/Resources/TAAZ?c=3&p=82
  # http://linnsoft.com/tour/techind/obVol.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic-obv.htm
  
  price <- try.xts(price, error=FALSE)
  volume <- try.xts(volume, error=FALSE)

  if(!(is.xts(price) && is.xts(volume))) {
    price <- as.vector(price)
    volume <- as.vector(volume)
  }
  obv <- c( volume[1], ifelse( ROC(price) > 0, volume, -volume )[-1] )
  obv <- cumsum( obv )

  if(is.xts(obv)) {
    obv <- xts(obv,index(price))
    colnames(obv) <- 'obv'
  }
  
  reclass( obv, price )
}
