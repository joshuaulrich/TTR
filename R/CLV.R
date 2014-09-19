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

#'Close Location Value
#'
#'The Close Location Value (CLV) relates the day's close to its trading range.
#'
#'The CLV will fall in a range of -1 to +1.  If the CLV is +/-1, the close is
#'at the high/low; if the CLV is 0, the close is directly between the high and
#'low.
#'
#'@param HLC Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.
#'@return A object of the same class as \code{HLC} or a vector (if
#'\code{try.xts} fails) containing the Close Location Values of a
#'High-Low-Close price series.
#'@author Joshua Ulrich
#'@seealso See \code{\link{chaikinAD}}, which uses CLV.
#'@references The following site(s) were used to code/document this
#'indicator:\cr
#'\url{http://stockcharts.com/education/IndicatorAnalysis/indic_AccumDistLine.html}\cr
#'@keywords ts
#'@examples
#'
#' data(ttrc)
#' clv <- CLV(ttrc[,c("High","Low","Close")])
#'
#'@export
"CLV" <-
function(HLC) {

  # Close Location Value

  HLC <- try.xts(HLC, error=as.matrix)
  clv <- ((HLC[,3]-HLC[,2]) - (HLC[,1]-HLC[,3])) / (HLC[,1]-HLC[,2])

  # Account for H=L=C
  clv[is.nan(clv) | is.infinite(clv)] <- 0

  if(is.xts(clv)) colnames(clv) <- 'clv'
  reclass( clv, HLC )
}
