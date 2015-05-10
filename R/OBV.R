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

#'On Balance Volume (OBV)
#'
#'On Balance Volume (OBV) is a measure of the money flowing into or out of a
#'security.  It is similar to Chaikin Accumulation / Distribution.
#'
#'OBV is calculated by adding (subtracting) each day's volume to a running
#'cumulative total when the security's price closes higher (lower).
#'
#'@param price Price series that is coercible to xts or matrix.
#'@param volume Volume series that is coercible to xts or matrix, that
#'corresponds to price object.
#'@return A object of the same class as \code{price} and \code{volume} or a
#'vector (if \code{try.xts} fails) containing the OBV values.
#'@note OBV is usually compared with the price chart of the underlying security
#'to look for divergences/confirmation.
#'@author Joshua Ulrich
#'@seealso See \code{\link{chaikinAD}}.
#'@references The following site(s) were used to code/document this
#'indicator:\cr \url{http://www.fmlabs.com/reference/OBV.htm}\cr
#'\url{http://www.equis.com/Customer/Resources/TAAZ?c=3&p=82}\cr
#'\url{http://linnsoft.com/tour/techind/obVol.htm}\cr
#'\url{http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:on_balance_volume_obv}\cr
#'@keywords ts
#'@examples
#'
#' data(ttrc)
#' obv <- OBV(ttrc[,"Close"], ttrc[,"Volume"])
#'
#'@export
"OBV" <-
function(price, volume) {

  # On Balance Volume
  
  price <- try.xts(price, error=as.matrix)
  volume <- try.xts(volume, error=as.matrix)

  if(!(is.xts(price) && is.xts(volume))) {
    price <- as.vector(price)
    volume <- as.vector(volume)
  }
  prChg <- ROC(price)
  obv <- c( volume[1], ifelse( prChg > 0, volume, -volume )[-1] )
  # OBV[t] = OBV[t-1] if price change is equal to zero
  obv[abs(prChg) < sqrt(.Machine$double.eps)] <- 0
  obv <- cumsum( obv )

  if(is.xts(obv)) {
    obv <- xts(obv,index(price))
    colnames(obv) <- 'obv'
  }
  
  reclass( obv, price )
}
