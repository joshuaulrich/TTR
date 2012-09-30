#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2007-2012  Joshua M. Ulrich
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

'DonchianChannel' <-
function(HL, n=10, include.lag=FALSE) {

  # Donchian Channel

  # http://www.linnsoft.com/tour/techind/donch.htm

  # Notes from John Bollinger:
  #
  # "In the old paper-calculation days you would calculate the numbers
  #  after the close by hand and for use in the next day's trading to gauge
  #  the "n-day" breakouts and you would have used n-days worth of data the
  #  calc. Thus an n-day calc with a lag of one would be consistent with
  #  practice in Donchian's day. (Total window of n+1.) Another example are
  #  the floor traders numbers or pivots, which are calculated from the
  #  prior period's data for use on the current period. In both case
  #  including the current period in the calculation would not be correct."

  HL <- try.xts(HL, error=as.matrix)

  if(!(NCOL(HL) %in% c(1,2))) {
    stop("Price series must be either High-Low, or Close/univariate.")
  }
  if(NCOL(HL)==2) {
      hi <- HL[,1]
      lo <- HL[,2]
  } else {
      hi <- HL
      lo <- HL
  }

  high <- runMax(hi,n)
  low  <- runMin(lo,n)
  mid  <- (high+low)/2

  result <- cbind(high,mid,low)
  colnames(result) <- c("high","mid","low")

  if(include.lag) {
    # use lag.xts in case 'result' is a matrix
    result <- lag.xts(result)
  }
  
  reclass(result, HL)
}
