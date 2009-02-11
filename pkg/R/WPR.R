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

"WPR" <-
function(HLC, n=14) {

  # William's Percent R (similar to Stochastics' fast %K)

  # http://www.fmlabs.com/reference/WilliamsR.htm
  # http://www.equis.com/Customer/Resources/TAAZ?c=3&p=126
  # http://linnsoft.com/tour/techind/willR.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_williamsR.html

  HLC <- try.xts(HLC, error=as.matrix)
  
  # Calculation if HLC series is given
  if(NCOL(HLC)==3) {
    high  <- HLC[,1]
    low   <- HLC[,2]
    close <- HLC[,3]
  } else

  # Calculation if price vector is given
  if(NCOL(HLC)==1) {
    high  <- HLC
    low   <- HLC
    close <- HLC
  } else

  stop("Price series must be either High-Low-Close, or Close")

  hmax <- runMax(high, n)
  lmin <- runMin( low, n)

  pctR <- (hmax - close) / (hmax - lmin)

  reclass( pctR, HLC )
}
