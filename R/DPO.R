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

#'De-Trended Price Oscillator
#'
#'The Detrended Price Oscillator (DPO) removes the trend in prices - or other
#'series - by subtracting a moving average of the price from the price.
#'
#'The Detrended Price shows cycles and overbought / oversold conditions.
#'
#'@param x Price, volume, etc. series that is coercible to xts or matrix.
#'@param n Number of periods for moving average.
#'@param maType A function or a string naming the function to be called.
#'@param shift The number of periods to shift the moving average.
#'@param percent logical; if \code{TRUE}, the percentage difference between the
#'slow and fast moving averages is returned, otherwise the difference between
#'the respective averages is returned.
#'@param \dots Other arguments to be passed to the \code{maType} function.
#'@return A object of the same class as \code{x} or a vector (if \code{try.xts}
#'fails) containing the DPO values.
#'@note
#'DPO does not extend to the last date because it is based on a displaced moving
#'average. The calculation shifts the results \code{shift} periods, so the last
#'\code{shift} periods will be zero.\cr
#'As stated above, the DPO can be used on any univariate series, not just price.
#'@author Joshua Ulrich
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.  See \code{\link{MACD}} for a general
#'oscillator.
#'@references The following site(s) were used to code/document this
#'indicator:\cr
#'\url{http://www.fmlabs.com/reference/DPO.htm}\cr
#'\url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=48}\cr
#'\url{http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:detrended_price_osci}\cr
#'@keywords ts
#'@examples
#'
#' data(ttrc)
#' priceDPO <- DPO(ttrc[,"Close"])
#' volumeDPO <- DPO(ttrc[,"Volume"])
#'
#'@export
"DPO" <-
function(x, n=10, maType, shift=n/2+1, percent=FALSE, ...) {

  # De-Trended Price Oscillator

  x <- try.xts(x, error=as.matrix)

  maArgs <- list(n=n, ...)
  # Default MA
  if(missing(maType)) {
    maType <- 'SMA'
  }

  mavg <- do.call( maType, c( list(x), maArgs ) )
  mavg <- lag.xts(mavg, -shift)

  if(percent) {
    DPO <- 100 * ( x[,1] / mavg - 1 )
  } else {
    DPO <- x[,1] - mavg
  }

  reclass( DPO, x )
}
