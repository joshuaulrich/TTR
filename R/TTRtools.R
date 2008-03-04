#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"lags" <-
function(x, n=1) {

  # Calculate lags of a series

  x <- as.matrix(x)
  if( is.null(colnames(x)) ) colnames(x) <- paste("V",1:NCOL(x),sep="")

  out <- embed(x, lag+1)
  if(lag==1)     lag.names <- 1      else
  if(NCOL(x)==1) lag.names <- 1:lag  else  lag.names <- rep(1:lag,NCOL(x))

  colnames(out) <- c( colnames(x), paste(colnames(x), sort(lag.names), sep=".") )

  return( out )
}

#-------------------------------------------------------------------------#
"growth" <-
function(price, signals, ...) {

  # Calculate growth of $1 for a series of returns (and signals).

  if(missing(signals)) {
    signals <- rep(1,NROW(price))
  } else {
    signals <- as.vector(signals)
  }
  price  <- as.vector(price)
  growth <- cumprod( 1 + ROC(price, ...) * signals )

  return( growth )
}
