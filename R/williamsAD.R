#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"williamsAD" <-
function(HLC) {

  # Williams Accumulation/Distribution

  # http://www.fmlabs.com/reference/WilliamsAD.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=125

  HLC <- as.matrix(HLC)

  # Calculate change in close, and true high/low
  dCl <- momentum(HLC[,3], 1, na.pad=TRUE)
  atr <- ATR(HLC)
  
  # Calculate AD
  ad <- rep(0,NROW(HLC))
  ad <- HLC[,3] - ifelse( dCl > 0, atr[,"trueLow"], atr[,"trueHigh"] )
  ad <- cumsum( ad )

  return( ad )
}
