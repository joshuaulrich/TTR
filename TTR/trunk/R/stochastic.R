#-------------------------------------------------------------------------#
# TTR, copyright (C) Joshua M. Ulrich, 2007                               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

"stochastic" <-
function(HLC, n.fastK=14, ma.fastD=list("SMA", n=3), ma.slowD=ma.fastD) {

  # Stochastics

  # http://www.fmlabs.com/reference/StochasticOscillator.htm
  # http://www.equis.com/Customer/Resources/TAAZ?c=3&p=106
  # http://linnsoft.com/tour/techind/stoc.htm
  # http://stockcharts.com/education/IndicatorAnalysis/indic_stochasticOscillator.html

  HLC <- as.matrix(HLC)

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

  hmax <- runMax(high, n.fastK)
  lmin <- runMin( low, n.fastK)

  fastK <- (close - lmin) / (hmax - lmin)
  fastD <- do.call( ma.fastD[[1]], c( list(fastK), ma.fastD[-1] ) )
  slowD <- do.call( ma.slowD[[1]], c( list(fastD), ma.slowD[-1] ) )

  return( cbind( fastK, fastD, slowD ) )
}

#-------------------------------------------------------------------------#

"SMI" <-
function(HLC, n=13, ma.slow=list("EMA", n=25), ma.fast=list("EMA", n=2),
         ma.sig=list("EMA", n=9)) {

  # Stochastic Momentum Index
  # Not Validated

  # http://www.fmlabs.com/reference/default.htm?url=SMI.htm
  # The median in the SMI formula on the above site is incorrect.

  HLC <- as.matrix(HLC)

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

  hmax <- runMax(high, n)
  lmin <- runMin( low, n)
  hmax <- ifelse( is.na(hmax), high, hmax )
  lmin <- ifelse( is.na(lmin),  low, lmin )

  HLdiff <- hmax - lmin
  Cdiff  <- close - ( hmax + lmin ) / 2

  num1 <- do.call( ma.slow[[1]], c( list(Cdiff ), ma.slow[-1] ) )
  den1 <- do.call( ma.slow[[1]], c( list(HLdiff), ma.slow[-1] ) )
  num2 <- do.call( ma.fast[[1]], c( list( num1 ), ma.fast[-1] ) )
  den2 <- do.call( ma.fast[[1]], c( list( den1 ), ma.fast[-1] ) )

  SMI <- 100 * ( num2 / ( den2 / 2 ) )
  signal <- do.call( ma.sig[[1]], c( list(SMI), ma.sig[-1] ) )

  return( cbind( SMI, signal ) )
}
