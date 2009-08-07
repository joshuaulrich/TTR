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

"aroon" <-
function(HL, n=20) {

  # Aroon up, down, and oscillator.

  # http://www.fmlabs.com/reference/Aroon.htm
  # http://www.fmlabs.com/reference/AroonOscillator.htm
  # http://www.linnsoft.com/tour/techind/aroon.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic-Aroon.htm

  HL <- try.xts(HL, error=as.matrix)

  aroonUp <- aroonDn <- vector("numeric",NROW(HL))

  # Calculation if price vector is given
  if(NCOL(HL)==1) {
    high <- HL
    low  <- HL
  } else

  # Calculation if HL series is given
  if(NCOL(HL)==2) {
    high <- HL[,1]
    low  <- HL[,2]
  } else

  stop("Price series must be either High-Low, or Close")

  # Find max and min of price series over past (n+1) days
  # It must be (n+1) to cover today, and the past n days
  hmax <- runMax(high, n+1)
  lmin <- runMin( low, n+1)

  # Calculate Aroon UP and DOWN
  for(i in (n+1):NROW(HL)) {
    aroonUp[i] <- 100 * ( max((0:n)[high[(i-n):i] %in% hmax[i]]) / n )
    aroonDn[i] <- 100 * ( max((0:n)[ low[(i-n):i] %in% lmin[i]]) / n )
  }

  oscillator <- aroonUp - aroonDn
  result <- cbind( aroonUp, aroonDn, oscillator )
  colnames(result) <- c( "aroonUp", "aroonDn", "oscillator" )

  reclass( result, HL )
}
