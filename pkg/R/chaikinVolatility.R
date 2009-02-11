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

"chaikinVolatility" <-
function(HL, n=10, maType, ...) {

  # Chaikin Volatility

  # http://www.fmlabs.com/reference/ChaikinVolatility.htm
  # http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=120

  HL <- try.xts(HL, error=as.matrix)

  maArgs <- list(n=n, ...)
  # Default MA
  if(missing(maType)) {
    maType <- 'EMA'
  }

  mavg <- do.call( maType, c( list(HL[,1]-HL[,2]), maArgs ) )

  volatility <- ROC( mavg, n, type="discrete" )

  reclass(volatility, HL)
}
