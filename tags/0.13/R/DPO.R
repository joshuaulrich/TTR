"DPO" <-
function(x, ma=list("SMA", n=10), shift=ma$n/2+1, percent=FALSE) {

  # De-Trended Price Oscillator

  # http://www.fmlabs.com/reference/DPO.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=48

  x <- as.vector(x)

  mavg <- do.call( ma[[1]], c( list(x), ma[-1] ) )
  mavg <- c( mavg[-c(1:shift)], rep(0, shift) )

  if(percent) {
    DPO <- 100 * ( x / mavg - 1 )
  } else {
    DPO <- x - mavg
  }

  DPO[(NROW(DPO)-shift+1):NROW(DPO)] <- 0

  return( DPO )
}
