#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

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

  if( !all.equal(NROW(n), NROW(wts), NROW(nROC)) )
    stop("'n', 'nROC', and 'wts' must be the same length.")

  #price <- as.vector(price)
  ret <- NULL

  # Default MA
  if(missing(maType)) {
    maType <- 'SMA'
  }

  # Case of two different 'maType's for both MAs.
  if( is.list(maType) ) {

    # If MA function has 'n' arg, see if it's populated in maType;
    # if it isn't, populate it with formal 'n'
    for(i in 1:length(maType)) {
      if( !is.null( formals(maType[[i]])$n ) && is.null( maType[[i]]$n ) ) {
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

  kst <- 100 * rowSums(ret)

  if( is.list(maType) ) {
    sigMA <- length(maType)
    signal <- do.call( maType[[sigMA]][[1]], c( list(kst), maType[[sigMA]][-1] ) )
  } else {
    signal <- do.call( maType, c( list(kst), list(n=nSig, ...) ) )
  }

  return( cbind( kst, signal ) )
}
