#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2007-2013  Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 2 of the License, or
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

#'Stochastic Oscillator / Stochastic Momentum Index
#'
#'The stochastic oscillator is a momentum indicator that relates the location
#'of each day's close relative to the high/low range over the past \code{n}
#'periods.  Developed by George C.  Lane in the late 1950s.  The SMI relates
#'the close to the midpoint of the high/low range.  Developed by William Blau
#'in 1993.
#'
#'If a High-Low-Close series is provided, the indicator is calculated using the
#'high/low values.  If a vector is provided, the calculation only uses that
#'series.  This allows stochastics to be calculated for: (1) series that have
#'no HLC definition (e.g. foreign exchange), and (2) stochastic indicators
#'(e.g. stochastic RSI - see examples).
#'
#'The \code{smooth} argument is the number of periods of internal smoothing to
#'apply to the differences in the high-low-close range before calculating Fast
#'K.  Thanks to Stanley Neo for the suggestion.
#'
#'@aliases stochastics stochastic stoch SMI %K %D
#'@param HLC Object that is coercible to xts or matrix and contains
#'High-Low-Close prices.  If only a univariate series is given, it will be
#'used.  See details.
#'@param n Number of periods to use.
#'@param nFastK Number of periods for fast \%K (i.e. the number of past periods
#'to use).
#'@param nFastD Number of periods for fast \%D (i.e. the number smoothing
#'periods to apply to fast \%K).
#'@param nSlowD Number of periods for slow \%D (i.e. the number smoothing
#'periods to apply to fast \%D).
#'@param smooth Number of internal smoothing periods to be applied before
#'calculating FastK. See Details.
#'@param nFast Number of periods for initial smoothing.
#'@param nSlow Number of periods for double smoothing.
#'@param nSig Number of periods for signal line.
#'@param maType Either:
#' \enumerate{
#'   \item A function or a string naming the function to be called.
#'   \item A \emph{list} with the first component like (1) above, and
#'     additional parameters specified as \emph{named} components.
#'     See Examples.
#' }
#'@param bounded Logical, should current period's values be used in the
#'calculation?
#'@param \dots Other arguments to be passed to the \code{maType} function in
#'case (1) above.
#'@return A object of the same class as \code{HLC} or a matrix (if
#'\code{try.xts} fails) containing the columns:
#' \describe{
#'    \item{ fastK }{ Stochastic Fast \%K }
#'    \item{ fastD }{ Stochastic Fast \%D }
#'    \item{ slowD }{ Stochastic Slow \%D }
#'    \item{ SMI }{ Stochastic Momentum Index }
#'    \item{ signal }{ Stochastic Momentum Index signal line }
#' }
#'@note The calculation for William's \%R is similar to that of stochastics'
#'fast \%K.
#'
#'The value for fast \%K will be 0.5 whenever the highest high and
#'lowest low are the same over the last \code{n} periods.
#'
#'The stochastic oscillator and SMI calculate relative value of the close
#'versus the high/low range and the midpoint of the high/low range,
#'respectively.
#'
#'The stochastic oscillator and the stochastic momentum index are interpreted
#'similarly.  Readings below 20 (above 80) are considered oversold
#'(overbought).  However, readings below 20 (above 80) are not necessarily
#'bearish (bullish).  Lane believed some of the best sell (buy) signals
#'occurred when the oscillator moved from overbought (oversold) back below 80
#'(above 20).
#'
#'For the stochastic oscillator, buy (sell) signals can also be given when \%K
#'crosses above (below) \%D.  Crossover signals are quite frequent however,
#'which may result in whipsaws.
#'@author Joshua Ulrich
#'@seealso See \code{\link{EMA}}, \code{\link{SMA}}, etc. for moving average
#'options; and note Warning section.  See \code{\link{WPR}} to compare it's
#'results to fast \%K.
#'@references The following site(s) were used to code/document these
#'indicators:
#'\cr Stochastic Oscillator:\cr
#'\url{http://www.fmlabs.com/reference/StochasticOscillator.htm}\cr
#'\url{http://www.equis.com/Customer/Resources/TAAZ?c=3&p=106}\cr
#'\url{http://linnsoft.com/tour/techind/stoc.htm}\cr
#'\url{http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:stochastic_oscillator_fast_slow_and_full}\cr
#'\cr SMI:\cr
#'\url{http://www.fmlabs.com/reference/default.htm?url=SMI.htm}\cr
#'@keywords ts
#'@examples
#'
#' data(ttrc)
#' stochOSC <- stoch(ttrc[,c("High","Low","Close")])
#' stochWPR <- WPR(ttrc[,c("High","Low","Close")])
#'
#' plot(tail(stochOSC[,"fastK"], 100), type="l",
#'     main="Fast %K and Williams %R", ylab="",
#'     ylim=range(cbind(stochOSC, stochWPR), na.rm=TRUE) )
#' lines(tail(stochWPR, 100), col="blue")
#' lines(tail(1-stochWPR, 100), col="red", lty="dashed")
#'
#' stoch2MA <- stoch( ttrc[,c("High","Low","Close")],
#'     maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA)) )
#'
#' SMI3MA <- SMI(ttrc[,c("High","Low","Close")],
#'     maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA)) )
#'
#' stochRSI <- stoch( RSI(ttrc[,"Close"]) )
#'@rdname stochastics
#'@export
"stoch" <-
function(HLC, nFastK=14, nFastD=3, nSlowD=3, maType, bounded=TRUE, smooth=1, ...) {

  # Stochastics

  HLC <- try.xts(HLC, error=as.matrix)

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

  num <- close - lmin
  den <- hmax - lmin

  if(missing(maType)) {
    maType <- 'SMA'
  }
  
  # Case of two different 'maType's for both MAs.
  # e.g. stoch(price, 14, 3, 3,
  #           maType=list(maUp=list(EMA,ratio=1/5), maDown=list(WMA,wts=1:10)) )
  if( is.list(maType) ) {

    # Make sure maType is a list of lists
    maTypeInfo <- sapply(maType,is.list)
    if( !(all(maTypeInfo) && length(maTypeInfo) == 3) ) {
      stop("If \'maType\' is a list, you must specify\n ",
      "*three* MAs (see Examples section of ?stochastics)")
    }

    # If MA function has 'n' arg, see if it's populated in maType;
    # if it isn't, populate it with function's formal 'n'
    if( !is.null( formals(maType[[1]][[1]])$n ) && is.null( maType[[1]]$n ) ) {
      maType[[1]]$n <- nFastD
    }
    if( !is.null( formals(maType[[2]][[1]])$n ) && is.null( maType[[2]]$n ) ) {
      maType[[2]]$n <- nSlowD
    }
    if( !is.null( formals(maType[[3]][[1]])$n ) && is.null( maType[[3]]$n ) ) {
      maType[[3]]$n <- smooth
    }
    
    numMA <- do.call( maType[[3]][[1]], c( list(num), maType[[3]][-1] ) )
    denMA <- do.call( maType[[3]][[1]], c( list(den), maType[[3]][-1] ) )

    fastK <- numMA / denMA
    fastK[is.nan(fastK)] <- 0.5
    fastD <- do.call( maType[[1]][[1]], c( list(fastK), maType[[1]][-1] ) )
    slowD <- do.call( maType[[2]][[1]], c( list(fastD), maType[[2]][-1] ) )
  }
  
  # Case of one 'maType' for both MAs.
  # e.g. stoch(price, 14, 3, 3, maType="WMA", wts=volume )
  else {
  
    numMA <- do.call( maType, c( list(num), list(n=smooth) ) )
    denMA <- do.call( maType, c( list(den), list(n=smooth) ) )

    fastK <- numMA / denMA
    fastK[is.nan(fastK)] <- 0.5
    fastD <- do.call( maType, c( list(fastK), list(n=nFastD, ...) ) )
    slowD <- do.call( maType, c( list(fastD), list(n=nSlowD, ...) ) )

  }

  result <- cbind( fastK, fastD, slowD )
  colnames(result) <- c( "fastK", "fastD", "slowD" )

  reclass(result, HLC)
}

#-------------------------------------------------------------------------#

#'@rdname stochastics
#'@export
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

    # Make sure maType is a list of lists
    maTypeInfo <- sapply(maType,is.list)
    if( !(all(maTypeInfo) && length(maTypeInfo) == 3) ) {
      stop("If \'maType\' is a list, you must specify\n ",
      "*three* MAs (see Examples section of ?SMI)")
    }

    # If MA function has 'n' arg, see if it's populated in maType;
    # if it isn't, populate it with function's formal 'n'
    if( !is.null( formals(maType[[1]][[1]])$n ) && is.null( maType[[1]]$n ) ) {
      maType[[1]]$n <- nFast
    }
    if( !is.null( formals(maType[[2]][[1]])$n ) && is.null( maType[[2]]$n ) ) {
      maType[[2]]$n <- nSlow
    }
    if( !is.null( formals(maType[[3]][[1]])$n ) && is.null( maType[[3]]$n ) ) {
      maType[[3]]$n <- nSig
    }
    
    num1 <- do.call( maType[[1]][[1]], c( list(Cdiff ), maType[[1]][-1] ) )
    den1 <- do.call( maType[[1]][[1]], c( list(HLdiff), maType[[1]][-1] ) )
    num2 <- do.call( maType[[2]][[1]], c( list( num1 ), maType[[2]][-1] ) )
    den2 <- do.call( maType[[2]][[1]], c( list( den1 ), maType[[2]][-1] ) )
  
    SMI <- 100 * ( num2 / ( den2 / 2 ) )
    signal <- do.call( maType[[3]][[1]], c( list(SMI), maType[[3]][-1] ) )

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

  result <- cbind( SMI, signal )
  colnames(result) <- c( "SMI", "signal" )
  
  reclass( result, HLC )
}
