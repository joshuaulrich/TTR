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

"EMV" <-
function(HL, volume, n=9, maType, vol.divisor=10000, ...) {

  # Arms' Ease of Movement Value

  # http://www.fmlabs.com/reference/ArmsEMV.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=51
  # http://linnsoft.com/tour/techind/arms.htm

  if( missing(HL) | missing(volume) )
    stop("High-Low matrix (HL) and volume vector must be specified.")

  HL <- try.xts(HL, error=as.matrix)
  volume <- try.xts(volume, error=as.matrix)

  if(!(is.xts(HL) && is.xts(volume))) {
    HL <- as.matrix(HL)
    volume <- as.matrix(volume)
  }

  mid     <- ( HL[,1] + HL[,2] ) / 2
  volume  <- volume / vol.divisor

  emv    <- momentum(mid, n=1, na.pad=TRUE) / ( volume / ( HL[,1] - HL[,2] ) )

  maArgs <- list(n=n, ...)
  # Default MA
  if(missing(maType)) {
    maType <- 'SMA'
  }

  maEMV <- do.call( maType, c( list(emv), maArgs ) )

  result <- cbind(emv,maEMV)
  colnames(result) <- c('emv','maEMV')
  
  reclass( result, HL )
}
