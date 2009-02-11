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

"OBV" <-
function(price, volume) {

  # On Balance Volume

  # http://www.fmlabs.com/reference/OBV.htm
  # http://www.equis.com/Customer/Resources/TAAZ?c=3&p=82
  # http://linnsoft.com/tour/techind/obVol.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic-obv.htm
  
  price <- try.xts(price, error=as.matrix)
  volume <- try.xts(volume, error=as.matrix)

  if(!(is.xts(price) && is.xts(volume))) {
    price <- as.vector(price)
    volume <- as.vector(volume)
  }
  obv <- c( volume[1], ifelse( ROC(price) > 0, volume, -volume )[-1] )
  obv <- cumsum( obv )

  if(is.xts(obv)) {
    obv <- xts(obv,index(price))
    colnames(obv) <- 'obv'
  }
  
  reclass( obv, price )
}
