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

"MFI" <-
function(HLC, volume, n=14) {

  # Money Flow Index

  # http://www.fmlabs.com/reference/default.htm?url=MoneyFlowIndex.htm
  # http://www.linnsoft.com/tour/techind/mfi.htm
  # http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:money_flow_index_mfi

  HLC <- try.xts(HLC, error=as.matrix)
  volume <- try.xts(volume, error=as.matrix)

  if(!(is.xts(HLC) && is.xts(volume))) {
    HLC <- as.matrix(HLC)
    volume <- as.matrix(volume)
  }

  if(NCOL(HLC)==3) {
    if(is.xts(HLC)) {
      HLC <- xts(rowMeans(HLC),index(HLC))
    } else {
      HLC <- rowMeans(HLC)
    }
  } else
  if(NCOL(HLC)!=1) {
    stop("Price series must be either High-Low-Close, or Close/univariate.")
  }

  if(is.xts(HLC)) {
    priceLag <- lag(HLC)
  } else {
    priceLag <- c( NA, HLC[-NROW(HLC)] )
  }

  # Calculate Money Flow
  mf <- HLC * volume
  # Calculate positive and negative Money Flow
  pmf <- ifelse( HLC > priceLag, mf, 0 )
  nmf <- ifelse( HLC < priceLag, mf, 0 )

  # Calculate Money Ratio and Money Flow Index
  mr <- runSum( pmf, n ) / runSum( nmf, n )
  mfi <- 100 - ( 100 / ( 1 + mr ) )
  if(is.xts(mfi)) colnames(mfi) <- 'mfi'

  reclass( mfi, HLC )
}
