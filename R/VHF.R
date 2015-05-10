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

#'Vertical Horizontal Filter
#'
#'The Vertical Horizontal Filter (VHF) attempts to identify starting and ending
#'trends.  Developed by Adam White.
#'
#'The VHF is calculated by subtracting the \code{n}-period lowest low from the
#'\code{n}-period highest high and dividing that result by the \code{n}-period
#'rolling sum of the close price changes.
#'
#'@param price Object that is coercible to xts or matrix and contains a Close
#'price series, or a High-Low-Close price series.
#'@param n Number of periods to use.
#'@return A object of the same class as \code{price} or a vector (if
#'\code{try.xts} fails) containing the VHF values.
#'@note If Close prices are given, the function calculates the max/min using
#'only those prices (the default).  If HLC prices are given, the function
#'calculates the max/min using the high/low prices (added for flexibility).
#'@author Joshua Ulrich
#'@seealso See \code{\link{aroon}}, \code{\link{CCI}}, \code{\link{ADX}},
#'\code{\link{TDI}}, \code{\link{GMMA}} for other indicators that measure trend
#'direction/strength.
#'@references The following site(s) were used to code/document this
#'indicator:\cr
#'\url{http://www.equis.com/Customer/Resources/TAAZ?c=3&p=119}\cr
#'@keywords ts
#'@examples
#'
#' data(ttrc)
#' vhf.close <- VHF(ttrc[,"Close"])
#' vhf.hilow <- VHF(ttrc[,c("High","Low","Close")])
#'
#'@export
"VHF" <-
function(price, n=28) {

  # Vertical Horizontal Filter

  price <- try.xts(price, error=as.matrix)

  # Calculation if price series is given
  if(NCOL(price)==1) {
    high  <- price
    low   <- price
    close <- price
  } else

  # Calculation if HLC series is given
  if(NCOL(price)==3) {
    high  <- price[,1]
    low   <- price[,2]
    close <- price[,3]
  } else

  stop("Price series must be either Close, or High-Low-Close")

  # Find highest max, and lowest min of price series
  hmax  <- runMax( high, n)
  lmin  <- runMin(  low, n)
  denom <- abs( momentum(close, n=1, na.pad=TRUE) )

  VHF <- ( hmax - lmin ) / runSum(denom, n)

  reclass(VHF, price)
}
