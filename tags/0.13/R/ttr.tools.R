######################################################################
# calculate rolling functions
# TO DO: add capability to index "..." args, if possible.
######################################################################
"rollFUN" <-
function(x, n, FUN, ...) {

  x  <- as.matrix(x)
  xr <- rep(NA,NROW(x))

  for(i in n:NROW(xr)) {
    xr[i] <- do.call( FUN, list( x[(i-n+1):i,], ... ) )
  }
  return( xr )
}

######################################################################
# Calculate a Welles Wilder style sum of a series
######################################################################
"wilderSum" <-
function(x, n=10) {

  x   <- as.vector(x)
  sum <- x

  for(i in 2:NROW(x)) {
    sum[i] <- x[i] + sum[i-1] * (n-1)/n
  }
  return( sum )
}

######################################################################
# Calculate lags of a series
######################################################################
"lags" <-
function(x, n=1) {

  x <- as.matrix(x)
  if( is.null(colnames(x)) ) colnames(x) <- paste("V",1:ncol(x),sep="")

  out <- embed(x, lag+1)
  if(lag==1)     lag.names <- 1      else
  if(ncol(x)==1) lag.names <- 1:lag  else  lag.names <- rep(1:lag,ncol(x))

  colnames(out) <- c( colnames(x), paste(colnames(x), sort(lag.names), sep=".") )

  return( out )
}

######################################################################
# Calculate growth of $1 for a series of returns (and signals).
######################################################################
"growth" <-
function(price, signals, ...) {

  if(missing(signals)) {
    signals <- rep(1,NROW(price))
  } else {
    signals <- as.vector(signals)
  }
  price  <- as.vector(price)
  growth <- cumprod( 1 + ROC(price, ...) * signals )

  return( growth )
}
