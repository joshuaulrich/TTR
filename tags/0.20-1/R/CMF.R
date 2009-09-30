#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2007-2008  Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
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

"CMF" <-
function(HLC, volume, n=20) {

  # Chaikin Money Flow

  # http://www.fmlabs.com/reference/ChaikinMoneyFlow.htm
  # http://www.linnsoft.com/tour/techind/cmf.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_ChaikinMoneyFlow1.html

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
