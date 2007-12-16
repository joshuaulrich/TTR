#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"RSI" <- 
function(price, ma.up=list("EMA", n=14, wilder=TRUE), ma.down=ma.up) {

  # Relative Strength Index

  # http://www.fmlabs.com/reference/RSI.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=100
  # http://linnsoft.com/tour/techind/rsi.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_RSI.html

  # Stochastic RSI

  # http://www.fmlabs.com/reference/StochRSI.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_stochRSI.html

  up <- momentum(price, n=1, na=NA)
  dn <- ifelse(up<0, abs(up), 0)
  up <- ifelse(up>0,     up , 0)

  mavg.up <- do.call( ma.up[[1]]  , c( list(up), ma.up[-1]   ) )
  mavg.dn <- do.call( ma.down[[1]], c( list(dn), ma.down[-1] ) )

  rsi <- 100 * mavg.up / ( mavg.up + mavg.dn )

  return( rsi )
}
