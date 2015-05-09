#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2007-2013  Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 2 of the License, or
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

#'MACD Oscillator
#'
#'The MACD was developed by Gerald Appel and is probably the most popular price
#'oscillator.  The MACD function documented in this page compares a fast moving
#'average (MA) of a series with a slow MA of the same series.  It can be used
#'as a generic oscillator for any univariate series, not only price.
#'
#'The MACD function either subtracts the fast MA from the slow MA, or finds the
#'rate of change between the fast MA and the slow MA.
#'
#'@param x Object that is coercible to xts or matrix; usually price, but can be
#'volume, etc.
#'@param nFast Number of periods for fast moving average.
#'@param nSlow Number of periods for slow moving average.
#'@param nSig Number of periods for signal moving average.
#'@param maType Either:
#' \enumerate{
#'   \item A function or a string naming the function to be called.
#'   \item A \emph{list} with the first component like (1) above, and
#'     additional parameters specified as \emph{named} components.
#'     See Examples.
#' }
#'@param percent logical; if \code{TRUE}, the percentage difference between the
#'fast and slow moving averages is returned, otherwise the difference between
#'the respective averages is returned.
#'@param \dots Other arguments to be passed to the \code{maType} function in
#'case (1) above.
#'@return A object of the same class as \code{x} or a matrix (if \code{try.xts}
#'fails) containing the columns:
#' \describe{
#'  \item{ macd }{ The price (volume, etc.) oscillator. }
#'  \item{ signal }{ The oscillator signal line (a moving average of the oscillator). }
#' }
#'@note The MACD is a special case of the general oscillator applied to price.
#'The MACD can be used as a general oscillator applied to any series. Time
#'periods for the MACD are often given as 26 and 12, but the function
#'originally used exponential constants of 0.075 and 0.15, which are closer to
#'25.6667 and 12.3333 periods.
#'@author Joshua Ulrich
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.
#'@references The following site(s) were used to code/document this
#'indicator:
#'\cr Moving Average Convergence/Divergence (MACD):\cr
#'\url{http://www.fmlabs.com/reference/MACD.htm}\cr
#'\url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=66}\cr
#'\url{http://linnsoft.com/tour/techind/macd.htm}\cr
#'\url{http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:moving_average_convergence_divergence_macd}\cr
#'\cr Price Oscillator:\cr
#'\url{http://www.fmlabs.com/reference/PriceOscillator.htm}\cr
#'\url{http://www.fmlabs.com/reference/PriceOscillatorPct.htm}\cr
#'\url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=94}\cr
#'\url{http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:price_oscillators_ppo}\cr
#'\cr Volume Oscillator:\cr
#'\url{http://www.fmlabs.com/reference/PVO.htm}\cr
#'\url{http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=122}\cr
#'@keywords ts
#'@examples
#'
#' data(ttrc)
#'
#' macd  <- MACD( ttrc[,"Close"], 12, 26, 9, maType="EMA" )
#' macd2 <- MACD( ttrc[,"Close"], 12, 26, 9,
#'          maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA)) )
#'
#'@export
"MACD" <-
function(x, nFast=12, nSlow=26, nSig=9, maType, percent=TRUE, ...) {

  # Oscillators

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
    
    mavg.fast <- do.call( maType[[1]][[1]], c( list(x), maType[[1]][-1] ) )
    mavg.slow <- do.call( maType[[2]][[1]], c( list(x), maType[[2]][-1] ) )

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
