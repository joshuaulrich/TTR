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

"ADX" <-
function(HLC, n=14, maType, ...) {

  # Welles Wilder's Directional Movement Index

  # http://www.fmlabs.com/reference/DI.htm
  # http://www.fmlabs.com/reference/DX.htm
  # http://www.fmlabs.com/reference/ADX.htm
  # http://www.fmlabs.com/reference/ADXR.htm
  # http://www.equis.com/Customer/Resources/TAAZ/Default.aspx?c=3&p=49
  # http://linnsoft.com/tour/techind/dirInd.htm
  # http://linnsoft.com/tour/techind/adx.htm
  # http://linnsoft.com/tour/techind/adxr.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_ADX.html

  HLC <- try.xts(HLC, error=as.matrix)
  dH  <- momentum(HLC[,1])
  dL  <- -momentum(HLC[,2])

  DMIp <- ifelse( dH==dL | (dH< 0 & dL< 0), 0, ifelse( dH >dL, dH, 0 ) )
  DMIn <- ifelse( dH==dL | (dH< 0 & dL< 0), 0, ifelse( dH <dL, dL, 0 ) )

  TR    <- ATR(HLC)[,"tr"]
  TRsum <- wilderSum(TR, n=n)

  DIp <- 100 * wilderSum(DMIp, n=n) / TRsum
  DIn <- 100 * wilderSum(DMIn, n=n) / TRsum

  DX  <- 100 * ( abs(DIp - DIn) / (DIp + DIn) )

  maArgs <- list(n=n, ...)
  
  # Default Welles Wilder EMA
  if(missing(maType)) {
    maType <- 'EMA'
    maArgs$wilder <- TRUE
  }

  ADX <- do.call( maType, c( list(DX), maArgs ) )

  result <- cbind( DIp, DIn, DX, ADX )
  colnames(result) <- c( "DIp", "DIn", "DX", "ADX" )

  reclass(result, HLC)
}
