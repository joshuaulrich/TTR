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

#'Moving Averages
#'
#'Calculate various moving averages (MA) of a series.
#'
#'\code{SMA} calculates the arithmetic mean of the series over the past
#'\code{n} observations.
#'
#'\code{EMA} calculates an exponentially-weighted mean, giving more weight to
#'recent observations.  See Warning section below.
#'
#'\code{WMA} is similar to an EMA, but with linear weighting if the length of
#'\code{wts} is equal to \code{n}.  If the length of \code{wts} is equal to the
#'length of \code{x}, the WMA will use the values of \code{wts} as weights.
#'
#'\code{DEMA} is calculated as: \code{DEMA = (1 + v) * EMA(x,n) -
#'EMA(EMA(x,n),n) * v} (with the corresponding \code{wilder} and \code{ratio}
#'arguments).
#'
#'\code{EVWMA} uses volume to define the period of the MA.
#'
#'\code{ZLEMA} is similar to an EMA, as it gives more weight to recent
#'observations, but attempts to remove lag by subtracting data prior to
#'\code{(n-1)/2} periods (default) to minimize the cumulative effect.
#'
#'\code{VWMA} and \code{VWAP} calculate the volume-weighted moving average
#'price.
#'
#'\code{VMA} calculate a variable-length moving average based on the absolute
#'value of \code{w}.  Higher (lower) values of \code{w} will cause \code{VMA}
#'to react faster (slower).
#'
#'\code{HMA} a WMA of the difference of two other WMAs, making it very
#'reponsive.
#'
#'\code{ALMA} inspired by Gaussian filters. Tends to put less weight on most
#'recent observations, reducing tendency to overshoot.
#'
#'@aliases MovingAverages SMA EMA WMA DEMA GD T3 EVWMA ZLEMA VWAP VWMA VMA MA
#'@param x Price, volume, etc. series that is coercible to xts or matrix.
#'@param price Price series that is coercible to xts or matrix.
#'@param volume Volume series that is coercible to xts or matrix, that
#'corresponds to price series, or a constant.  See Notes.
#'@param n Number of periods to average over.
#'@param v The 'volume factor' (a number in [0,1]).  See Notes.
#'@param w Vector of weights (in [0,1]) the same length as \code{x}.
#'@param wts Vector of weights.  Length of \code{wts} vector must equal the
#'length of \code{x}, or \code{n} (the default).
#'@param wilder logical; if \code{TRUE}, a Welles Wilder type EMA will be
#'calculated; see notes.
#'@param ratio A smoothing/decay ratio.  \code{ratio} overrides \code{wilder}
#'in \code{EMA}, and provides additional smoothing in \code{VMA}.
#'@param offset Percentile at which the center of the distribution should occur.
#'@param sigma Standard deviation of the distribution.
#'@param \dots any other passthrough parameters
#'@return A object of the same class as \code{x} or \code{price} or a vector
#'(if \code{try.xts} fails) containing the columns:
#' \describe{
#'    \item{SMA}{ Simple moving average. }
#'    \item{EMA}{ Exponential moving average. }
#'    \item{WMA}{ Weighted moving average. }
#'    \item{DEMA}{ Double-exponential moving average. }
#'    \item{EVWMA}{ Elastic, volume-weighted moving average. }
#'    \item{ZLEMA}{ Zero lag exponential moving average. }
#'    \item{VWMA}{ Volume-weighed moving average (same as \code{VWAP}). }
#'    \item{VWAP}{ Volume-weighed average price (same as \code{VWMA}). }
#'    \item{VWA}{ Variable-length moving average. }
#'    \item{HMA}{ Hull moving average. }
#'    \item{ALMA}{ Arnaud Legoux moving average. }
#' }
#'@note For \code{EMA}, \code{wilder=FALSE} (the default) uses an exponential
#'smoothing ratio of \code{2/(n+1)}, while \code{wilder=TRUE} uses Welles
#'Wilder's exponential smoothing ratio of \code{1/n}.
#'
#'Since \code{WMA} can accept a weight vector of length equal to the length of
#'\code{x} or of length \code{n}, it can be used as a regular weighted moving
#'average (in the case \code{wts=1:n}) or as a moving average weighted by
#'volume, another indicator, etc.
#'
#'Since \code{DEMA} allows adjusting \code{v}, it is technically Tim Tillson's
#'generalized DEMA (GD).  When \code{v=1} (the default), the result is the
#'standard DEMA.  When \code{v=0}, the result is a regular EMA.  All other
#'values of \code{v} return the GD result.  This function can be used to
#'calculate Tillson's T3 indicator (see example below).  Thanks to John Gavin
#'for suggesting the generalization.
#'
#'For \code{EVWMA}, if \code{volume} is a series, \code{n} should be chosen so
#'the sum of the volume for \code{n} periods approximates the total number of
#'outstanding shares for the security being averaged.  If \code{volume} is a
#'constant, it should represent the total number of outstanding shares for the
#'security being averaged.
#'@section Warning : Some indicators (e.g. EMA, DEMA, EVWMA, etc.) are
#'calculated using the indicators' own previous values, and are therefore
#'unstable in the short-term.  As the indicator receives more data, its output
#'becomes more stable.  See example below.
#'@author Joshua Ulrich, Ivan Popivanov (HMA, ALMA)
#'@seealso See \code{\link{wilderSum}}, which is used in calculating a Welles
#'Wilder type MA.
#'@references The following site(s) were used to code/document this
#'indicator:\cr \url{http://www.fmlabs.com/reference/ExpMA.htm}\cr
#'\url{http://www.fmlabs.com/reference/WeightedMA.htm}\cr
#'\url{http://www.fmlabs.com/reference/DEMA.htm}\cr
#'\url{http://www.fmlabs.com/reference/T3.htm}\cr
#'\url{http://linnsoft.com/tour/techind/evwma.htm}\cr
#'\url{http://www.fmlabs.com/reference/ZeroLagExpMA.htm}\cr
#'\url{http://www.fmlabs.com/reference/VIDYA.htm}\cr
#'\url{http://www.traderslog.com/hullmovingaverage}\cr
#'\url{http://www.arnaudlegoux.com/}\cr
#'@keywords ts
#'@examples
#'
#' data(ttrc)
#' ema.20 <-   EMA(ttrc[,"Close"], 20)
#' sma.20 <-   SMA(ttrc[,"Close"], 20)
#' dema.20 <-  DEMA(ttrc[,"Close"], 20)
#' evwma.20 <- EVWMA(ttrc[,"Close"], ttrc[,"Volume"], 20)
#' zlema.20 <- ZLEMA(ttrc[,"Close"], 20)
#' alma <- ALMA(ttrc[,"Close"])
#' hma <- HMA(ttrc[,"Close"])
#'
#' ## Example of Tim Tillson's T3 indicator
#' T3 <- function(x, n=10, v=1) DEMA(DEMA(DEMA(x,n,v),n,v),n,v)
#' t3 <- T3(ttrc[,"Close"])
#' 
#' ## Example of short-term instability of EMA
#' ## (and other indicators mentioned above)
#' x <- rnorm(100)
#' tail( EMA(x[90:100],10), 1 )
#' tail( EMA(x[70:100],10), 1 )
#' tail( EMA(x[50:100],10), 1 )
#' tail( EMA(x[30:100],10), 1 )
#' tail( EMA(x[10:100],10), 1 )
#' tail( EMA(x[ 1:100],10), 1 )
#' 
#'@rdname MovingAverages
#'@export
"SMA" <-
function(x, n=10, ...) {

  # Simple Moving Average

  ma <- runMean( x, n )
  
  if(!is.null(dim(ma))) {
    colnames(ma) <- "SMA"
  }

  return(ma)
}

#-------------------------------------------------------------------------#

#'@rdname MovingAverages
#'@export
"EMA" <-
function (x, n=10, wilder=FALSE, ratio=NULL, ...) {

  # Exponential Moving Average

  x <- try.xts(x, error=as.matrix)
  if( n < 1 || n > NROW(x) )
    stop("Invalid 'n'")
  if(NCOL(x) > 1) {
    stop("ncol(x) > 1. EMA only supports univariate 'x'")
  }
  if( any(nNonNA <- n > colSums(!is.na(x))) )
    stop("n > number of non-NA values in column(s) ",
         paste(which(nNonNA), collapse=", "))

  # Check for non-leading NAs
  # Leading NAs are handled in the C code
  x.na <- naCheck(x, n)
  
  # If ratio is specified, and n is not, set n to approx 'correct'
  # value backed out from ratio
  if(missing(n) && !missing(ratio))
    n <- trunc(2/ratio - 1)

  # Determine decay ratio
  if(is.null(ratio)) {
    if(wilder) ratio <- 1/n
    else       ratio <- 2/(n+1)
  }

  # Call C routine
  ma <- .Call("ema", x, n, ratio, PACKAGE = "TTR")

  ma <- reclass(ma,x)
  
  if(!is.null(dim(ma))) {
    colnames(ma) <- "EMA"
  }

  return(ma)
  
}

#-------------------------------------------------------------------------#

#'@rdname MovingAverages
#'@export
"DEMA" <-
function(x, n=10, v=1, wilder=FALSE, ratio=NULL) {

  # Double Exponential Moving Average
  # Thanks to John Gavin for the v-factor generalization

  if(v < 0 || v > 1) {
    stop("Please ensure 0 <= v <= 1")
  }

  dema <- (1 + v) * EMA(x,n,wilder,ratio) -
    EMA(EMA(x,n,wilder,ratio),n,wilder,ratio) * v

  if(!is.null(dim(dema))) {
    colnames(dema) <- "DEMA"
  }

  return(dema)
}

#-------------------------------------------------------------------------#

#'@rdname MovingAverages
#'@export
"WMA" <-
function(x, n=10, wts=1:n, ...) {

  # Weighted Moving Average

  x <- try.xts(x, error=as.matrix)
  wts <- try.xts(wts, error=as.matrix)

  if( !any( NROW(wts) == c( NROW(x), n ) ) )
    stop("Length of 'wts' must equal the length of 'x' or 'n'")
  if( n < 1 || n > NROW(x) )
    stop("Invalid 'n'")
  if(NCOL(x) > 1 || NCOL(wts) > 1) {
    stop("ncol(x) > 1 or ncol(wts) > 1. WMA only supports univariate 'x' and 'w'")
  }

  # Count NAs, ensure they're only at beginning of data, then remove.
  NAx <- sum( is.na(x) )
  NAw <- sum( is.na(wts) )
  NAs <- max( NAx, NAw )
  if( NAs > 0 ) {
    if( any( is.na(  x[-(1:NAx)]) ) )
      stop("'x' contains non-leading NAs")
    if( any( is.na(wts[-(1:NAw)]) ) )
      stop("'wts' contains non-leading NAs")
  }
  
  if( NROW(wts) == n ) {
    
    x <- na.omit(x)
    NAs <- NAx

    if( any(is.na(wts)) )
      stop("'wts' vector of length 'n' cannot have NA values")

    # Call Fortran routine
    ma <- .Fortran( "wma", ia = as.double(x),
                           lia = as.integer(NROW(x)),
                           wts = as.double(wts),
                           n = as.integer(n),
                           oa = as.double(x),
                           loa = as.integer(NROW(x)),
                           PACKAGE = "TTR",
                           DUP = TRUE )$oa
   
  } else {
    
    xw <- na.omit( cbind(x, wts) )
    ma <- runSum( xw[,1]*xw[,2], n) / runSum(xw[,2], n)
  }

  # replace 1:(n-1) with NAs and prepend NAs from original data
  ma[1:(n-1)] <- NA
  ma <- c( rep( NA, NAs ), ma )

  if(!is.null(dim(ma))) {
    colnames(ma) <- "WMA"
  }

  reclass(ma,x)
}

#-------------------------------------------------------------------------#

#'@rdname MovingAverages
#'@export
"EVWMA" <-
function(price, volume, n=10, ...) {

  # Elastic, Volume-Weighted Moving Average

  price <- try.xts(price, error=as.matrix)
  volume <- try.xts(volume, error=as.matrix)

  if( !any( NROW(volume) == c( NROW(price), 1 ) ) )
    stop("Length of 'volume' must equal 1 or the length of 'price'")
  if( n < 1 || n > NROW(price) )
    stop("Invalid 'n'")
  if(NCOL(price) > 1 || NCOL(volume) > 1) {
    stop("ncol(price) > 1 or ncol(volume) > 1.",
         " EVWMA only supports univariate 'price' and 'volume'")
  }

  pv <- cbind(price, volume)

  if( any(nNonNA <- n > colSums(!is.na(pv))) )
    stop("n > number of non-NA values in ",
         paste(c("price","volume")[which(nNonNA)], collapse=", "))

  # Check for non-leading NAs
  # Leading NAs are handled in the C code
  pv.na <- naCheck(pv, n)

  # Call C routine
  ma <- .Call("evwma", pv[,1], pv[,2], n, PACKAGE = "TTR")

  if(!is.null(dim(ma))) {
    colnames(ma) <- "EVWMA"
  }

  # Convert back to original class
  reclass(ma, price)
}

#-------------------------------------------------------------------------#

#'@rdname MovingAverages
#'@export
"ZLEMA" <-
function (x, n=10, ratio=NULL, ...) {

  # Zero-Lag Exponential Moving Average

  x <- try.xts(x, error=as.matrix)
  if(NCOL(x) > 1) {
    stop("ncol(x) > 1. ZLEMA only supports univariate 'x'")
  }
  
  # Count NAs, ensure they're only at beginning of data, then remove.
  NAs <- sum( is.na(x) )
  if( NAs > 0 ) {
    if( any( is.na(x[-(1:NAs)]) ) )
      stop("Series contains non-leading NAs")
  }
  x   <- na.omit(x)

  # Initialize ma vector
  ma <- rep(1, NROW(x))
  ma[n] <- mean(x[1:n])

  # Determine decay ratio
  if(is.null(ratio)) {
    ratio <- 2/(n+1)
  }

  # Call Fortran routine
  ma <- .Fortran( "zlema", ia = as.double(x),
                           lia = as.integer(NROW(x)),
                           n = as.integer(n),
                           oa = as.double(ma),
                           loa = as.integer(NROW(ma)),
                           ratio = as.double(ratio),
                           PACKAGE = "TTR",
                           DUP = TRUE )$oa

  # replace 1:(n-1) with NAs and prepend NAs from original data
  ma[1:(n-1)] <- NA
  ma <- c( rep( NA, NAs ), ma ) 
  
  if(!is.null(dim(ma))) {
    colnames(ma) <- "ZLEMA"
  }

  reclass(ma,x)
}

#-------------------------------------------------------------------------#

#'@rdname MovingAverages
#'@export VWAP VWMA
"VWAP" <- "VWMA" <-
function(price, volume, n=10, ...) {

  # Volume-weighted average price
  # Volume-weighted moving average

  res <- WMA(price, n=n, volume)
  
  if(!is.null(dim(res))) {
    colnames(res) <- "VWAP"
  }

  return(res)
}

#-------------------------------------------------------------------------#

#'@rdname MovingAverages
#'@export
"VMA" <-
function (x, w, ratio=1, ...) {

  # Variable Moving Average

  x <- try.xts(x, error=as.matrix)
  w <- try.xts(w, error=as.matrix)

  if( NROW(w) != NROW(x) )
    stop("Length of 'w' must equal the length of 'x'")

  if(NCOL(x) > 1 || NCOL(w) > 1) {
    stop("ncol(x) > 1 or ncol(w) > 1. VMA only supports univariate 'x' and 'w'")
  }

  # Check for non-leading NAs
  # Leading NAs are handled in the C code
  x.na <- naCheck(x, 1)
  w.na <- naCheck(w, 1)
  
  # Call C routine
  ma <- .Call("vma", x, abs(w), ratio, PACKAGE = "TTR")

  if(!is.null(dim(ma))) {
    colnames(ma) <- "VMA"
  }

  reclass(ma,x)
}

#-------------------------------------------------------------------------#

#'@rdname MovingAverages
#'@export
"HMA" <-
function(x, n=20, ...) {

  # Hull Moving Average

  reclass(WMA(2*WMA(x, n=n/2, ...) - WMA(x, n=n, ...), n=trunc(sqrt(n)), ...), x)
}

#-------------------------------------------------------------------------#

#'@rdname MovingAverages
#'@export
"ALMA" <-
function(x, n=9, offset=0.85, sigma=6, ...) {

  # ALMA (Arnaud Legoux Moving Average)

  if(offset < 0 || offset > 1) {
    stop("Please ensure 0 <= offset <= 1")
  }
  if(sigma <= 0)
    stop("sigma must be > 0")

  m <- floor(offset*(n-1))
  s <- n/sigma
  wts <- exp(-((seq(0,n-1)-m)^2)/(2*s*s))
  sumWeights <- sum(wts)
  if(sumWeights != 0)
    wts <- wts/sumWeights
  alma <- rollapply(x, width=n, FUN=function(xx) sum(xx*wts), align="right")
  reclass(alma, x)
}
