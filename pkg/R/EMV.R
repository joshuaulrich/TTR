#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"EMV" <-
function(HL, volume, n=9, maType, vol.divisor=10000, ...) {

  # Arms' Ease of Movement Value

  # http://www.fmlabs.com/reference/ArmsEMV.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=51
  # http://linnsoft.com/tour/techind/arms.htm

  if( missing(HL) | missing(volume) )
    stop("High-Low matrix (HL) and volume vector must be specified.")

  mid     <- ( HL[,1] + HL[,2] ) / 2
  volume  <- volume / vol.divisor

  emv    <- momentum(mid, n=1, na.pad=TRUE) / ( volume / ( HL[,1] - HL[,2] ) )

  maArgs <- list(n=n, ...)
  # Default MA
  if(missing(maType)) {
    maType <- 'SMA'
  }

  maEMV <- do.call( maType, c( list(emv), maArgs ) )

  return( cbind( emv, maEMV ) )
}
