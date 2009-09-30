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

"KST" <-
function(price, n=c(10,10,10,15), nROC=c(10,15,20,30), nSig=9,
         maType, wts=1:NROW(n), ...) {

  # Know Sure Thing

  # Technical Analysis Explained: The Successful Investor's Guide to
  # Spotting Investment Trends and Turning Points
  # Martin J. Pring
  # http://www.pring.com/index.html
  # Daily:	http://www.pring.com/movieweb/daily_kst.htm
  # Long-Term:	http://www.pring.com/articles/article28.htm

  # Daily KST
  # MA(ROC(10)10) + MA(ROC(15)10) + MA(ROC(20)10) + MA(ROC(30)15)
  #
  # Intermediate KST
  # MA(ROC(10)10) + MA(ROC(13)13) + MA(ROC(15)15) + MA(ROC(20)20)
  #
  # Long-Term Monthly KST
  # MA(ROC(9)6) + MA(ROC(12)6) + MA(ROC(18)6) + MA(ROC(24)9)

  if( !all.equal(NROW(n), NROW(wts), NROW(nROC)) ) {
    stop("'n', 'nROC', and 'wts' must be the same length.")
  } else {
    N <- NROW(n)
  }

  #price <- as.vector(price)
  ret <- NULL

  # Default MA
  if(missing(maType)) {
    maType <- 'SMA'
  }

  # Case of two different 'maType's for both MAs.
  if( is.list(maType) ) {

    # Make sure maType is a list of lists
    maTypeInfo <- sapply(maType,is.list)
    if( !(all(maTypeInfo) && length(maTypeInfo) == N) ) {
      stop("If \'maType\' is a list, you must specify\n ",
      "the same number of MAs as elements in \'n\' and\n ",
      "\'nROC\' (see Examples section of ?KST)")
    }

    # If MA function has 'n' arg, see if it's populated in maType;
    # if it isn't, populate it with formal 'n'
    for(i in 1:length(maType)) {
      if( !is.null( formals(maType[[i]][[1]])$n ) && is.null( maType[[i]]$n ) ) {
        maType[[i]]$n <- n[i]
      }
      roc <- ROC(price, nROC[i], na.pad=TRUE)
      ma.roc <- do.call( maType[[i]][[1]], c( list(roc), maType[[i]][-1] ) ) * wts[i]
      ret <- cbind( ret, ma.roc )
    }

  }
  
  # Case of one 'maType' for both MAs.
  else {
  
    for(i in 1:NROW(n)) {
      roc <- ROC(price, nROC[i], na.pad=TRUE)
      ma.roc <- do.call( maType, c( list(roc), list(n=n[i], ...) ) ) * wts[i]
      ret <- cbind( ret, ma.roc )
    }

  }

  if(is.xts(ret)) {
    kst <- xts(rowSums(ret),index(ret))
  } else {
    kst <- 100 * rowSums(ret)
  }

  if( is.list(maType) ) {
    sigMA <- length(maType)
    signal <- do.call( maType[[sigMA]][[1]], c( list(kst), maType[[sigMA]][-1] ) )
  } else {
    signal <- do.call( maType, c( list(kst), list(n=nSig, ...) ) )
  }

  result <- cbind( kst, signal )
  colnames(result) <- c( "kst", "signal" )

  return( result )
}
