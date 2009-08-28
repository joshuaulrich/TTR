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

"VHF" <-
function(price, n=28) {

  # Vertical Horizontal Filter

  # http://www.equis.com/Customer/Resources/TAAZ?c=3&p=119

  price <- try.xts(price, error=as.matrix)

  # Calculation if price series is given
  if(NCOL(price)==1) {
    high  <- price
    low   <- price
    close <- price
  } else

  # Calculation if HLC series is given
  if(NCOL(price)==3) {
    high  <- price[,1]
    low   <- price[,2]
    close <- price[,3]
  } else

  stop("Price series must be either Close, or High-Low-Close")

  # Find highest max, and lowest min of price series
  hmax  <- runMax( high, n)
  lmin  <- runMin(  low, n)
  denom <- abs( momentum(close, n=1, na.pad=TRUE) )

  VHF <- ( hmax - lmin ) / runSum(denom, n)

  reclass(VHF, price)
}
