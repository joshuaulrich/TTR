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

"williamsAD" <-
function(HLC) {

  # Williams Accumulation/Distribution

  # http://www.fmlabs.com/reference/WilliamsAD.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=125

  HLC <- try.xts(HLC, error=as.matrix)

  # Calculate change in close, and true high/low
  dCl <- momentum(HLC[,3], 1)
  atr <- ATR(HLC)
  
  # Calculate AD
  ad <- HLC[,3] - ifelse( dCl > 0, atr[,"trueLow"], atr[,"trueHigh"] )
  ad[ dCl == 0 ] <- 0
  
  ad.na <- naCheck(ad)
  ad <- cumsum( ad[ad.na$nonNA] )
  ad <- c( rep( NA, ad.na$NAs ), ad )

  reclass(ad, HLC)
}
