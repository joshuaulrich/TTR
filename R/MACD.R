#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"MACD" <-
function(x, nFast=12, nSlow=26, nSig=9, maType, percent=TRUE, ...) {

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

  # Default MA
  if(missing(maType)) {
    maType <- 'EMA'
  }

  # Case of two different 'maType's for both MAs.
  if( is.list(maType) ) {

    # If MA function has 'n' arg, see if it's populated in maType;
    # if it isn't, populate it with function's formal 'n'
    if( !is.null( formals(maType[[1]])$n ) && is.null( maType[[1]]$n ) ) {
      maType[[1]]$n <- nFast
    }
    if( !is.null( formals(maType[[2]])$n ) && is.null( maType[[2]]$n ) ) {
      maType[[2]]$n <- nSlow
    }
    if( !is.null( formals(maType[[3]])$n ) && is.null( maType[[3]]$n ) ) {
      maType[[3]]$n <- nSig
    }
    
    mavg.slow <- do.call( maType[[1]][[1]], c( list(x), maType[[1]][-1] ) )
    mavg.fast <- do.call( maType[[2]][[1]], c( list(x), maType[[2]][-1] ) )

  }
  
  # Case of one 'maType' for both MAs.
  else {
  
    mavg.fast <- do.call( maType, c( list(x), list(n=nFast, ...) ) )
    mavg.slow <- do.call( maType, c( list(x), list(n=nSlow, ...) ) )

  }
  
  if(percent) {
    macd <- 100 * ( mavg.fast / mavg.slow - 1 )
  } else {
    macd <- mavg.fast - mavg.slow
  }

  if( is.list(maType) ) {
    signal <- do.call( maType[[3]][[1]], c( list( macd ), maType[[3]][-1] ) )
  } else
    signal <- do.call( maType, c( list( macd ), list(n=nSig, ...) ) )

  return( cbind( macd, signal ) )
}
