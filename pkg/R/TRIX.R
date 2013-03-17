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

#'Triple Smoothed Exponential Oscillator
#'
#'The TRIX indicator calculates the rate of change of a triple exponential
#'moving average.  Developed by Jack K. Hutson.
#'
#'The TRIX is calculated as follows:\cr 3MA = \code{MA}( \code{MA}(
#'\code{MA}(\code{price}) ) )\cr trix = 100 * [ 3MA(t) / 3MA(t-1) - 1 ]
#'
#'@param price Price series that is coercible to xts or matrix.
#'@param n Number of periods for moving average.
#'@param nSig Number of periods for signal line moving average.
#'@param maType Either:
#' \enumerate{
#'   \item A function or a string naming the function to be called.
#'   \item A \emph{list} with the first component like (1) above, and
#'     additional parameters specified as \emph{named} components.
#'     See Examples.
#' }
#'@param percent logical; if \code{TRUE}, the rate of change is calculated
#'using the \code{ROC} function, otherwise the \code{momentum} function is
#'used.
#'@param \dots Other arguments to be passed to the \code{maType} function in
#'case (1) above.
#'@return A object of the same class as \code{price} or a vector (if
#'\code{try.xts} fails) containing the TRIX values.
#'@note Buy/sell signals are generated when the TRIX crosses above/below zero.
#'A nine-period EMA of the TRIX is used as a default signal line.  Buy/sell
#'signals are generated when the TRIX crosses above/below the signal line and
#'is also above/below zero.
#'@author Joshua Ulrich
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.
#'@references The following site(s) were used to code/document this
#'indicator:\cr
#'\url{http://www.fmlabs.com/reference/default.htm?url=TRIX.htm}\cr
#'\url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=114}\cr
#'\url{http://www.linnsoft.com/tour/techind/trix.htm}\cr
#'\url{http://stockcharts.com/education/IndicatorAnalysis/indic_trix.htm}\cr
#'@keywords ts
#'@examples
#'
#' data(ttrc)
#' trix  <- TRIX(ttrc[,"Close"])
#' trix4 <- TRIX(ttrc[,"Close"],
#' maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA), list(DEMA)))
#'
#'@export
"TRIX" <-
function(price, n=20, nSig=9, maType, percent=TRUE, ...) {

  # Triple Smoothed Exponential Oscillator

  # Default MA
  if(missing(maType)) {
    maType <- 'EMA'
  }

  # Case of different 'maType's for all MAs.
  if( is.list(maType) ) {

    # Make sure maType is a list of lists
    maTypeInfo <- sapply(maType,is.list)
    if( !(all(maTypeInfo) && length(maTypeInfo) == 4) ) {
      stop("If \'maType\' is a list, you must specify\n ",
      "*four* MAs (see Examples section of ?TRIX)")
    }

    # If MA function has 'n' arg, see if it's populated in maType;
    # if it isn't, populate it with function's formal 'n'
    if( !is.null( formals(maType[[1]][[1]])$n ) && is.null( maType[[1]]$n ) ) {
      maType[[1]]$n <- n
    }
    if( !is.null( formals(maType[[2]][[1]])$n ) && is.null( maType[[2]]$n ) ) {
      maType[[2]]$n <- n
    }
    if( !is.null( formals(maType[[3]][[1]])$n ) && is.null( maType[[3]]$n ) ) {
      maType[[3]]$n <- n
    }
    if( !is.null( formals(maType[[4]][[1]])$n ) && is.null( maType[[4]]$n ) ) {
      maType[[4]]$n <- nSig
    }
    
    mavg1 <- do.call( maType[[1]][[1]], c( list(price), maType[[1]][-1] ) )
    mavg2 <- do.call( maType[[2]][[1]], c( list(mavg1), maType[[2]][-1] ) )
    mavg3 <- do.call( maType[[3]][[1]], c( list(mavg2), maType[[3]][-1] ) )

  }
  
  # Case of one 'maType' for all MAs.
  else {
  
    mavg1 <- do.call( maType, c( list(price), list(n=n, ...) ) )
    mavg2 <- do.call( maType, c( list(mavg1), list(n=n, ...) ) )
    mavg3 <- do.call( maType, c( list(mavg2), list(n=n, ...) ) )

  }

  if(percent) {
    TRIX <- 100 * ROC(mavg3, n=1, na.pad=TRUE, type="discrete")
  } else {
    TRIX <- momentum( mavg3, n=1, na.pad=TRUE )
  }
  
  if( is.list(maType) ) {
    signal <- do.call( maType[[4]][[1]], c( list(TRIX), maType[[4]][-1] ) )
  } else {
    signal <- do.call( maType, c( list(TRIX), list(n=n, ...) ) )
  }

  result <- cbind( TRIX, signal )
  colnames(result) <- c( "TRIX", "signal" )

  return( result )
}
