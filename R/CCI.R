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

#'Commodity Channel Index
#'
#'The Commodity Channel Index (CCI) attempts to identify starting and ending
#'trends.
#'
#'CCI relates the current price and the average of price over \code{n} periods.
#'The CCI usually falls in a channel of -100 to 100. A basic CCI trading system
#'is: Buy (sell) if CCI rises above 100 (falls below -100) and sell (buy) when
#'it falls below 100 (rises above -100).
#'
#'CCI is usually calculated using the typical price, but if a univariate series
#'(e.g. Close, Weighted Close, Median Price, etc.) is provided, it will be used
#'instead.
#'
#'@param HLC Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.  If only a univariate series is given, it will be
#'used.  See details.
#'@param n Number of periods for moving average.
#'@param maType A function or a string naming the function to be called.
#'@param c Constant to apply to the mean deviation.
#'@param \dots Other arguments to be passed to the \code{maType} function.
#'@return A object of the same class as \code{HLC} or a vector (if
#'\code{try.xts} fails) containing the CCI values.
#'@note If \code{HLC} is a High-Low-Close matrix, then typical price will be
#'calculated.  If \code{HLC} is a vector, then those values will be used
#'instead of the typical price.
#'@author Joshua Ulrich
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.  See \code{\link{aroon}},
#'\code{\link{ADX}}, \code{\link{TDI}}, \code{\link{VHF}}, \code{\link{GMMA}}
#'for other indicators that measure trend direction/strength.
#'@references The following site(s) were used to code/document this
#'indicator:\cr \url{http://www.fmlabs.com/reference/CCI.htm}\cr
#'\url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=42}\cr
#'\url{http://www.linnsoft.com/tour/techind/cci.htm}\cr
#'\url{http://stockcharts.com/education/IndicatorAnalysis/indic_CCI.html}\cr
#'@keywords ts
#'@examples
#'
#' data(ttrc)
#' cci <- CCI(ttrc[,c("High","Low","Close")])
#'
#'@export
"CCI" <-
function(HLC, n=20, maType, c=0.015, ...) {

  # Commodity Channel Index

  HLC <- try.xts(HLC, error=as.matrix)

  if(NCOL(HLC)==3) {
    if(is.xts(HLC)) {
      xa <- xcoredata(HLC)
      HLC <- xts(apply(HLC, 1, mean),index(HLC))
      xcoredata(HLC) <- xa
    } else {
      HLC <- apply(HLC, 1, mean)
    }
  } else
  if(NCOL(HLC)!=1) {
    stop("Price series must be either High-Low-Close, or Close/univariate.")
  }

  maArgs <- list(n=n, ...)
  # Default MA
  if(missing(maType)) {
    maType <- 'SMA'
  }
  
  mavg  <- do.call( maType, c( list(HLC), maArgs ) )
  meanDev <- runMAD( HLC, n, center=mavg, stat="mean" )

  cci <- ( HLC - mavg ) / ( c * meanDev )

  if(is.xts(cci)) {
    colnames(cci) <- "cci"
  }

  reclass(cci, HLC)
}
