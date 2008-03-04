#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"ZigZag" <- 
function(HL, retrace=0.10, change="percent") {

  # Zig Zag Indicator
  # Adapted from Alberto Santini's code

  # http://www.fmlabs.com/reference/default.htm?url=ZigZag.htm
  # http://www.linnsoft.com/tour/techind/zigzag.htm
  # http://www.linnsoft.com/tour/techind/zigosc.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=127

  # Calculation if HL series is given
  if(NCOL(HL)==2) {
    high  <- HL[,1]
    low   <- HL[,2]
  } else

  # Calculation if price vector is given
  if(NCOL(HL)==1) {
    high  <- HL
    low   <- HL
  } else

  stop("Price series must be either High-Low, or Univariate")
  
  # Initialize necessary parameters
  ch <- match.arg(change, c("percent","dollar"))=="percent"
  nn <- NROW(HL)
  zz <- rep(0, nn)

  # Call Fortran routine
  zz <- .Fortran("zigzag", iha   = as.double( high ),
                           ila   = as.double( low ),
                           la    = as.integer( nn ),
                           minch = as.double( retrace ),
                           ch    = as.integer( ch ),
                           zz    = as.double( zz ),
                           PACKAGE = "TTR" )$zz
  
  # Interpolate results
  zz <- ifelse( zz==0, NA, zz )
  zz <- approx( zz, xout=1:nn )$y

  return( zz )
}
