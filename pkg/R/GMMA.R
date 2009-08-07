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

"GMMA" <-
function(x, short=c(3,5,8,10,12,15), long=c(30,35,40,45,50,60), maType) {

  # Guppy Multiple Moving Average

  # http://www.investopedia.com/terms/g/guppy-multiple-moving-average.asp

  x <- try.xts(x, error=as.matrix)
  
  # Default MA
  if(missing(maType)) {
    maType <- 'EMA'
  }
  
  gmma <- sapply(c(short,long), function(g) do.call(maType, list(x,n=g)))
  colnames(gmma) <- c(paste('short lag',short),paste('long lag',long))
  
  reclass(gmma, x)
}
