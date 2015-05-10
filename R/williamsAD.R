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

#'Williams Accumulation / Distribution
#'
#'The Williams Accumulation / Distribution (AD) line is a measure of market
#'momentum.  Developed by Larry Williams.
#'
#'The Williams AD line differs from OBV and chaikinAD in that it doesn't take
#'volume into account.
#'
#'@param HLC Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.
#'@return A object of the same class as \code{HLC} or a vector (if
#'\code{try.xts} fails) containing the accumulation / distribution values.
#'@note The Accumulation/Distribution Line is interpreted by looking for a
#'divergence in the direction of the indicator relative to price.
#'@author Joshua Ulrich
#'@seealso See \code{\link{OBV}}, \code{\link{chaikinAD}}, and
#'\code{\link{ATR}}.
#'@references The following site(s) were used to code/document this
#'indicator:\cr
#'\url{http://www.fmlabs.com/reference/WilliamsAD.htm}\cr
#'\url{http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=125}\cr
#'@keywords ts
#'@examples
#'
#' data(ttrc)
#' ad <- williamsAD(ttrc[,c("High","Low","Close")])
#'
#'@export
"williamsAD" <-
function(HLC) {

  # Williams Accumulation/Distribution

  HLC <- try.xts(HLC, error=as.matrix)

  # Calculate change in close, and true high/low
  dCl <- momentum(HLC[,3], 1)
  atr <- ATR(HLC)
  
  # Calculate AD
  ad <- HLC[,3] - ifelse( dCl > 0, atr[,"trueLow"], atr[,"trueHigh"] )
  ad[ dCl == 0 ] <- 0
  
  ad.na <- naCheck(ad)
  ad <- cumsum( ad[ad.na$nonNA] )
  ad <- c( rep( NA, ad.na$NAs ), ad )

  reclass(ad, HLC)
}
