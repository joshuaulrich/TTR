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

#'Chaikin Money Flow
#'
#'Chaikin Money Flow compares total volume over the last \code{n} time periods
#'to total volume times the Close Location Value (CLV) over the last \code{n}
#'time periods.  Developed by Marc Chaikin.
#'
#'Chaikin Money Flow is calculated by taking dividing the sum of the Chaikin
#'Accumulation / Distribution line over the past \code{n} periods by the sum of
#'volume over the past \code{n} periods.
#'
#'@param HLC Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.
#'@param volume Vector or matrix of volume observations corresponding to the
#'\code{HLC} object.
#'@param n Number of periods to use.
#'@return A object of the same class as \code{HLC} and \code{volume} or a
#'vector (if \code{try.xts} fails) containing the Chaikin Money Flow values.
#'@note When Chaikin Money Flow is above/below +/- 0.25 it is a bullish/bearish
#'signal.  If Chaikin Money Flow remains below zero while the price is rising,
#'it indicates a probable reversal.
#'@author Joshua Ulrich
#'@seealso See \code{\link{CLV}}, and \code{\link{chaikinAD}}.
#'@references The following site(s) were used to code/document this
#'indicator:\cr \url{http://www.fmlabs.com/reference/ChaikinMoneyFlow.htm}\cr
#'\url{http://www.linnsoft.com/tour/techind/cmf.htm}\cr
#'\url{http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:chaikin_money_flow_cmf}\cr
#'@keywords ts
#'@examples
#'
#' data(ttrc)
#' cmf <- CMF(ttrc[,c("High","Low","Close")], ttrc[,"Volume"])
#'
#'@export
"CMF" <-
function(HLC, volume, n=20) {

  # Chaikin Money Flow

  HLC <- try.xts(HLC, error=as.matrix)
  volume <- try.xts(volume, error=as.matrix)

  if(!(is.xts(HLC) && is.xts(volume))) {
    clv <- CLV(as.matrix(HLC))
    volume <- as.matrix(volume)
  }
  clv <- CLV(HLC)

  cmf <- runSum(clv*volume, n) / runSum(volume, n)

  reclass(cmf, HLC)
}
