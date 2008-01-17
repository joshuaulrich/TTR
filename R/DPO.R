#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"DPO" <-
function(x, n=10, maType="SMA", shift=n/2+1, percent=FALSE, ...) {

  # De-Trended Price Oscillator

  # http://www.fmlabs.com/reference/DPO.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=48

  x <- as.vector(x)

  maArgs <- list(n=n, ...)
  mavg <- do.call( maType, c( list(x), maArgs ) )
  mavg <- c( mavg[-c(1:shift)], rep(0, shift) )

  if(percent) {
    DPO <- 100 * ( x / mavg - 1 )
  } else {
    DPO <- x - mavg
  }

  DPO[(NROW(DPO)-shift+1):NROW(DPO)] <- 0

  return( DPO )
}
