#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"stoch" <-
function(HLC, nFastK=14, nFastD=3, nSlowD=3, maType, bounded=TRUE, ...) {

  # Stochastics

  # http://www.fmlabs.com/reference/StochasticOscillator.htm
  # http://www.equis.com/Customer/Resources/TAAZ?c=3&p=106
  # http://linnsoft.com/tour/techind/stoc.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_stochasticOscillator.html

  # Calculation if HLC series is given
  if(NCOL(HLC)==3) {
    high  <- HLC[,1]
    low   <- HLC[,2]
    close <- HLC[,3]
  } else

  # Calculation if price vector is given
  if(NCOL(HLC)==1) {
    high  <- HLC
    low   <- HLC
    close <- HLC
  } else

  stop("Price series must be either High-Low-Close, or Close")

  if(bounded) {
    hmax <- runMax(high, nFastK)
    lmin <- runMin( low, nFastK)
  } else {
    hmax <- runMax(c(high[1],high[-NROW(HLC)]), nFastK)
    lmin <- runMax(c( low[1], low[-NROW(HLC)]), nFastK)
  }

  fastK <- (close - lmin) / (hmax - lmin)

  if(missing(maType)) {
    maType <- 'SMA'
  }
  
  # Case of two different 'maType's for both MAs.
  # e.g. stoch(price, 14, 3, 3,
  #           maType=list(maUp=list(EMA,ratio=1/5), maDown=list(WMA,wts=1:10)) )
  if( is.list(maType) ) {

    # If MA function has 'n' arg, see if it's populated in maType;
    # if it isn't, populate it with function's formal 'n'
    if( !is.null( formals(maType[[1]])$n ) && is.null( maType[[1]]$n ) ) {
      maType[[1]]$n <- nFastD
    }
    if( !is.null( formals(maType[[2]])$n ) && is.null( maType[[2]]$n ) ) {
      maType[[2]]$n <- nSlowD
    }
    
    fastD <- do.call( maType[[1]][[1]], c( list(fastK), maType[[1]][-1] ) )
    slowD <- do.call( maType[[2]][[1]], c( list(fastD), maType[[2]][-1] ) )
  }
  
  # Case of one 'maType' for both MAs.
  # e.g. stoch(price, 14, 3, 3, maType="WMA", wts=volume )
  else {
  
    fastD <- do.call( maType, c( list(fastK), list(n=nFastD, ...) ) )
    slowD <- do.call( maType, c( list(fastD), list(n=nSlowD, ...) ) )

  }

  return( cbind( fastK, fastD, slowD ) )
}

#-------------------------------------------------------------------------#

"SMI" <-
function(HLC, n=13, nFast=2, nSlow=25, nSig=9, maType, bounded=TRUE, ...) {

  # Stochastic Momentum Index
  # Not Validated

  # http://www.fmlabs.com/reference/default.htm?url=SMI.htm
  # The median in the SMI formula on the above site is incorrect.

  # Calculation if HLC series is given
  if(ncol(HLC)==3) {
    high  <- HLC[,1]
    low   <- HLC[,2]
    close <- HLC[,3]
  } else

  # Calculation if price vector is given
  if(ncol(HLC)==1) {
    high  <- HLC
    low   <- HLC
    close <- HLC
  } else

  stop("Price series must be either High-Low-Close, or Close")

  if(bounded) {
    hmax <- runMax(high, n)
    lmin <- runMin( low, n)
  } else {
    hmax <- runMax(c(high[1],high[-NROW(HLC)]), n)
    lmin <- runMax(c( low[1], low[-NROW(HLC)]), n)
  }
  hmax <- ifelse( is.na(hmax), high, hmax )
  lmin <- ifelse( is.na(lmin),  low, lmin )

  HLdiff <- hmax - lmin
  Cdiff  <- close - ( hmax + lmin ) / 2

  if(missing(maType)) {
    maType <- 'EMA'
  }
  
  # Case of two different 'maType's for both MAs.
  # e.g. SMI(price, 13, 2, 25, 9,
  #           maType=list(maUp=list(EMA,ratio=1/5), maDown=list(WMA,wts=1:10)) )
  if( is.list(maType) ) {

    # If MA function has 'n' arg, see if it's populated in maType;
    # if it isn't, populate it with function's formal 'n'
    if( !is.null( formals(maType[[1]])$n ) && is.null( maType[[1]]$n ) ) {
      maType[[1]]$n <- nFast
    }
    if( !is.null( formals(maType[[2]])$n ) && is.null( maType[[2]]$n ) ) {
      maType[[2]]$n <- nSlow
    }
    if( !is.null( formals(maType[[3]])$n ) && is.null( maType[[3]]$n ) ) {
      maType[[3]]$n <- nSig
    }
    
    num1 <- do.call( maType[[1]], c( list(Cdiff ), maType[[1]][-1] ) )
    den1 <- do.call( maType[[1]], c( list(HLdiff), maType[[1]][-1] ) )
    num2 <- do.call( maType[[2]], c( list( num1 ), maType[[2]][-1] ) )
    den2 <- do.call( maType[[2]], c( list( den1 ), maType[[2]][-1] ) )
  
    SMI <- 100 * ( num2 / ( den2 / 2 ) )
    signal <- do.call( maType[[3]], c( list(SMI), maType[[3]][-1] ) )

  }
  
  # Case of one 'maType' for both MAs.
  # e.g. SMI(price, 13, 2, 25, 9, maType="WMA", wts=volume )
  else {
  
    num1 <- do.call( maType, c( list(Cdiff ), list(n=nSlow, ... ) ) )
    den1 <- do.call( maType, c( list(HLdiff), list(n=nSlow, ... ) ) )
    num2 <- do.call( maType, c( list( num1 ), list(n=nFast, ... ) ) )
    den2 <- do.call( maType, c( list( den1 ), list(n=nFast, ... ) ) )
  
    SMI <- 100 * ( num2 / ( den2 / 2 ) )
    signal <- do.call( maType, c( list(SMI), list(n=nSig, ... ) ) )

  }

  return( cbind( SMI, signal ) )
}
