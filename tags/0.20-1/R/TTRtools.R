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

#-------------------------------------------------------------------------#

'naCheck' <-
function(x, n=0) {

  # Ensure NAs are only at beginning of data.
  if(is.null(dim(x)[2])) {
    NAs <- sum(is.na(x))
    if( NAs > 0 ) {
      if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
    }
  } else {
    NAs <- sum( rowSums(is.na(x)) > 0 )
    if( NAs > 0 ) {
      if( any( is.na(x[-(1:NAs),]) ) ) stop("Series contains non-leading NAs")
    }
  }
  
  res <- list()
  res$NAs <- NAs
  res$nonNA <- (1+NAs):NROW(x)
  res$beg <- n+NAs

  invisible(res)
}
