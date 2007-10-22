"EMV" <-
function(HL, volume, ma=list("SMA", n=9), vol.divisor=10000) {

  # Arms' Ease of Movement Value

  # http://www.fmlabs.com/reference/ArmsEMV.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=51
  # http://linnsoft.com/tour/techind/arms.htm

  if( missing(HL) | missing(volume) )
    stop("High-Low matrix (HL) and volume vector must be specified.")

  mid     <- ( HL[,1] + HL[,2] ) / 2
  volume  <- volume / vol.divisor

  emv    <- momentum(mid, n=1, na=0) / ( volume / ( HL[,1] - HL[,2] ) )
  ma.emv <- do.call( ma[[1]], c( list(emv), ma[-1] ) )

  return( cbind( emv, ma.emv ) )
}
