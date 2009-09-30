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

"DPO" <-
function(x, n=10, maType, shift=n/2+1, percent=FALSE, ...) {

  # De-Trended Price Oscillator

  # http://www.fmlabs.com/reference/DPO.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=48

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
