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

#'Relative Strength Index
#'
#'The Relative Strength Index (RSI) calculates a ratio of the recent upward
#'price movements to the absolute price movement.  Developed by J. Welles
#'Wilder.
#'
#'The RSI calculation is \code{RSI = 100 - 100 / ( 1 + RS )}, where \code{RS}
#'is the smoothed ratio of 'average' gains over 'average' losses.  The
#''averages' aren't true averages, since they're divided by the value of
#'\code{n} and not the number of periods in which there are  gains/losses.
#'
#'@param price Price series that is coercible to xts or matrix.
#'@param n Number of periods for moving averages.
#'@param maType Either:
#' \enumerate{
#'   \item A function or a string naming the function to be called.
#'   \item A \emph{list} with the first component like (1) above, and
#'     additional parameters specified as \emph{named} components.
#'     See Examples.
#' }
#'@param \dots Other arguments to be passed to the \code{maType} function in
#'case (1) above.
#'@return A object of the same class as \code{price} or a vector (if
#'\code{try.xts} fails) containing the RSI values.
#'@note The RSI is usually interpreted as an overbought/oversold (over 70 /
#'below 30) indicator.  Divergence with price may also be useful.  For example,
#'if price is making new highs/lows, but RSI is not, it could indicate a
#'reversal.
#'
#'You can calculate a stochastic RSI by using the function \code{\link{stoch}}
#'on RSI values.
#'@author Joshua Ulrich
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.  See \code{\link{CMO}} for a variation on
#'RSI.
#'@references The following site(s) were used to code/document this
#'indicator:
#'\cr Relative Strength Index:\cr
#'\url{http://www.fmlabs.com/reference/RSI.htm}\cr
#'\url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=100}\cr
#'\url{http://linnsoft.com/tour/techind/rsi.htm}\cr
#'\url{http://stockcharts.com/education/IndicatorAnalysis/indic_RSI.html}\cr
#'\cr Stochastic RSI:\cr
#'\url{http://www.fmlabs.com/reference/StochRSI.htm}\cr
#'\url{http://stockcharts.com/education/IndicatorAnalysis/indic_stochRSI.html}\cr
#'@keywords ts
#'@examples
#'
#' data(ttrc)
#' price <- ttrc[,"Close"]
#'
#' # Default case
#' rsi <- RSI(price)
#'
#' # Case of one 'maType' for both MAs
#' rsiMA1 <- RSI(price, n=14, maType="WMA", wts=ttrc[,"Volume"])
#'
#' # Case of two different 'maType's for both MAs
#' rsiMA2 <- RSI(price, n=14, maType=list(maUp=list(EMA,ratio=1/5),
#'              maDown=list(WMA,wts=1:10)))
#'
#'
#'@export
"RSI" <- 
function(price, n=14, maType, ...) {

  price <- try.xts(price, error=as.matrix)

  up <- momentum(price, n=1, na.pad=TRUE)
  which.dn <- which(up < 0)
  dn <- up*0
  dn[which.dn] <- -up[which.dn]
  up[which.dn] <- 0

  maArgs <- list(n=n, ...)
  # Default Welles Wilder EMA
  if(missing(maType)) {
    maType <- 'EMA'
    maArgs$wilder <- TRUE
  }

  # Case of two different 'maType's for both MAs.
  # e.g. RSI(price, n=14, maType=list(maUp=list(EMA,ratio=1/5), maDown=list(WMA,wts=1:10)) )
  if( is.list(maType) ) {

    # Make sure maType is a list of lists
    maTypeInfo <- sapply(maType,is.list)
    if( !(all(maTypeInfo) && length(maTypeInfo) == 2) ) {
      stop("If \'maType\' is a list, you must specify\n ",
      "*two* MAs (see Examples section of ?RSI)")
    }
    
    # If MA function has 'n' arg, see if it's populated in maType;
    # if it isn't, populate it with RSI's formal 'n'
    for(i in 1:length(maType)) {
      if( !is.null( formals(maType[[i]])$n ) && is.null( maType[[i]]$n ) ) {
        maType[[i]]$n <- n
      }
      mavgUp <- do.call( maType[[1]][[1]], c( list(up), maType[[1]][-1] ) )
      mavgDn <- do.call( maType[[2]][[1]], c( list(dn), maType[[2]][-1] ) )
    }
  }
  
  # Case of one 'maType' for both MAs.
  # e.g. RSI(price, n=14, maType="WMA", wts=volume )
  else {
  
    mavgUp <- do.call( maType, c( list(up), maArgs ) )
    mavgDn <- do.call( maType, c( list(dn), maArgs ) )
  }

  rsi <- 100 * mavgUp / ( mavgUp + mavgDn )

  reclass( rsi, price )
}
