#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"MACD" <-
function(x, ma.fast=list("EMA", n=12), ma.slow=list("EMA", n=26),
             ma.sig=list("EMA", n=9), percent=FALSE) {

  # Oscillators

  # Moving Average Convergence/Divergence (MACD)
  # http://www.fmlabs.com/reference/MACD.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=66
  # http://linnsoft.com/tour/techind/macd.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_MACD1.html

  # Price Oscillator
  # http://www.fmlabs.com/reference/PriceOscillator.htm
  # http://www.fmlabs.com/reference/PriceOscillatorPct.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=94
  # http://stockcharts.com/education/IndicatorAnalysis/indic_priceOscillator.html

  # Volume Oscillator
  # http://www.fmlabs.com/reference/PVO.htm
  # http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=122

  # WISHLIST:
  # Add capability to allow 'ma.slow' and 'ma.fast' to be vectors
  # containing MAs, which would allow the oscillator to be constructed
  # using MAs of different prices.

  mavg.slow <- do.call( ma.slow[[1]], c( list( x ), ma.slow[-1] ) )
  mavg.fast <- do.call( ma.fast[[1]], c( list( x ), ma.fast[-1] ) )

  if(percent) {
    macd <- 100 * ( mavg.fast / mavg.slow - 1 )
  } else {
    macd <- mavg.fast - mavg.slow
  }

  if(is.null(ma.sig)) {
    signal <- NULL
  } else
    signal <- do.call( ma.sig[[1]], c( list( macd ), ma.sig[-1] ) )

  return( cbind( macd, signal ) )
}
