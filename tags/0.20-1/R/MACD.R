#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2007-2008  Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

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

    # Make sure maType is a list of lists
    maTypeInfo <- sapply(maType,is.list)
    if( !(all(maTypeInfo) && length(maTypeInfo) == 3) ) {
      stop("If \'maType\' is a list, you must specify\n ",
      "*three* MAs (see Examples section of ?MACD)")
    }

    # If MA function has 'n' arg, see if it's populated in maType;
    # if it isn't, populate it with function's formal 'n'
    if( !is.null( formals(maType[[1]][[1]])$n ) && is.null( maType[[1]]$n ) ) {
      maType[[1]]$n <- nFast
    }
    if( !is.null( formals(maType[[2]][[1]])$n ) && is.null( maType[[2]]$n ) ) {
      maType[[2]]$n <- nSlow
    }
    if( !is.null( formals(maType[[3]][[1]])$n ) && is.null( maType[[3]]$n ) ) {
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

  result <- cbind( macd, signal )
  colnames(result) <- c( "macd", "signal" )
  
  return( result )
}
