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

#'Aroon
#'
#'The Aroon indicator attempts to identify starting trends.  The indicator
#'consists of up and down lines, which measure how long it has been since the
#'highest high/lowest low has occurred in the last \code{n} periods.  Developed
#'by Tushar Chande in 1995.
#'
#'Aroon up (down) is the elapsed time, expressed as a percentage, between today
#'and the highest (lowest) price in the last \code{n} periods.  If today's
#'price is a new high (low) Aroon up (down) will be 100. Each subsequent period
#'without another new high (low) causes Aroon up (down) to decrease by (1 /
#'\code{n}) x 100.
#'
#'@param HL Object that is coercible to xts or matrix and contains either a
#'High-Low price series, or a Close price series.
#'@param n Number of periods to use in the calculation.
#'@return A object of the same class as \code{HL} or a matrix (if
#'\code{try.xts} fails) containing the columns:
#' \describe{
#'  \item{ aroonUp }{ The Aroon up indicator. }
#'  \item{ aroonDn }{ The Aroon down indicator. }
#'  \item{ oscillator }{ The Aroon oscillator (\code{aroonUp - aroonDn}). }
#' }
#'@note If High-Low prices are given, the function calculates the max/min using
#'the high/low prices.  Otherwise the function calculates the max/min of the
#'single series.
#'
#'Up (down) trends are indicated when the aroonUp(Dn) is between 70 and 100.
#'Strong trends are indicated when when the aroonUp(Dn) is above 70 while the
#'aroonDn(Up) is below 30.  Also, crossovers may be useful.
#'@author Joshua Ulrich
#'@seealso See \code{\link{CCI}}, \code{\link{ADX}}, \code{\link{TDI}},
#'\code{\link{VHF}}, \code{\link{GMMA}} for other indicators that measure trend
#'direction/strength.
#'@references The following site(s) were used to code/document this
#'indicator:\cr \url{http://www.fmlabs.com/reference/Aroon.htm}\cr
#'\url{http://www.fmlabs.com/reference/AroonOscillator.htm}\cr
#'\url{http://www.linnsoft.com/tour/techind/aroon.htm}\cr
#'\url{http://stockcharts.com/education/IndicatorAnalysis/indic-Aroon.htm}\cr
#'@keywords ts
#'@examples
#'
#' ## Get Data and Indicator ##
#' data(ttrc)
#' trend <- aroon( ttrc[,c("High", "Low")], n=20 )
#'
#'@export
"aroon" <-
function(HL, n=20) {

  # Aroon up, down, and oscillator.

  HL <- try.xts(HL, error=as.matrix)

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

  # Calculate Aroon UP and DOWN
  aroonUp <- .Call("aroon_max", high, n, PACKAGE="TTR")
  aroonDn <- .Call("aroon_max", -low, n, PACKAGE="TTR")

  oscillator <- aroonUp - aroonDn
  result <- cbind( aroonUp, aroonDn, oscillator )
  colnames(result) <- c( "aroonUp", "aroonDn", "oscillator" )

  reclass( result, HL )
}
