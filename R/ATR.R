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

"ATR" <-
function(HLC, n=14, maType, ...) {

  # Average True Range / True Range

  # http://www.fmlabs.com/reference/TR.htm
  # http://www.fmlabs.com/reference/ATR.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=35
  # http://www.linnsoft.com/tour/techind/trueRange.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_ATR.html

  HLC <- try.xts(HLC, error=as.matrix)
  
  if(is.xts(HLC)) {
    closeLag <- lag(HLC[,3])
  } else {
    closeLag <- c( NA, HLC[-NROW(HLC),3] )
  }

  trueHigh <- pmax( HLC[,1], closeLag, na.rm=FALSE )
  trueLow  <- pmin( HLC[,2], closeLag, na.rm=FALSE )
  tr       <- trueHigh - trueLow

  maArgs <- list(n=n, ...)
  
  # Default Welles Wilder EMA
  if(missing(maType)) {
    maType <- 'EMA'
    maArgs$wilder <- TRUE
  }

  atr <- do.call( maType, c( list(tr), maArgs ) )

  result <- cbind( tr, atr, trueHigh, trueLow )
  colnames(result) <- c('tr','atr','trueHigh','trueLow')
  
  reclass( result, HLC )
}
