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

#'Analysis of Running/Rolling/Moving Windows
#'
#'Various functions to analyze data over a moving window of periods.
#'
#'
#'@aliases runFun runSum runMin runMax runMean runMedian runCov runCor runVar
#'runSD runMAD wilderSum
#'@param x Object coercible to xts or matrix.
#'@param y Object coercible to xts or matrix.
#'@param n Number of periods to use in the window or, if
#'\code{cumulative=TRUE}, the number of obversations to use before the first
#'result is returned.
#'@param cumulative Logical, use from-inception calculation?
#'@param sample Logical, sample covariance if \code{TRUE} (denominator of
#'\code{n-1})
#'@param use Only \code{"all.obs"} currently implemented.
#'@param non.unique One of 'mean', 'max', or 'min'; which compute their
#'respective statistics for the two middle values of even-sized samples.
#'@param center The values to use as the measure of central tendency, around
#'which to calculate deviations. The default (\code{NULL}) uses the median.
#'@param stat Statistic to calculate, one of 'median' or 'mean' (e.g. median
#'absolute deviation or mean absolute deviation, respectively.)
#'@param constant Scale factor applied to approximate the standard deviation.
#'@return A object of the same class as \code{x} and \code{y} or a vector (if
#'\code{try.xts} fails).
#' \describe{
#'  \item{runSum}{returns sums over a n-period moving window.}
#'  \item{runMin}{returns minimums over a n-period moving window.}
#'  \item{runMax}{returns maximums over a n-period moving window.}
#'  \item{runMean}{returns means over a n-period moving window.}
#'  \item{runMedian}{returns medians over a n-period moving window.}
#'  \item{runCov}{returns covariances over a n-period moving window.}
#'  \item{runCor}{returns correlations over a n-period moving window.}
#'  \item{runVar}{returns variances over a n-period moving window.}
#'  \item{runSD}{returns standard deviations over a n-period moving window.}
#'  \item{runMAD}{returns median/mean absolute deviations over a n-period moving window.}
#'  \item{wilderSum}{retuns a Welles Wilder style weighted sum over a n-period moving window.}
#' }
#' 
#'@author Joshua Ulrich
#'@keywords ts
#'@rdname runFun
#'@export
"runSum" <-
function(x, n=10, cumulative=FALSE) {

  x <- try.xts(x, error=as.matrix)

  if( n < 1 || n > NROW(x) ) stop("Invalid 'n'")

  if(NCOL(x) > 1) {
    stop("ncol(x) > 1. runSum only supports univariate 'x'")
  }

  # Count NAs, ensure they're only at beginning of data.
  NAs <- sum(is.na(x))
  if( NAs > 0 ) {
    if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
    if( NAs + n > NROW(x) ) stop("not enough non-NA values")
  }
  beg <- 1 + NAs
  len <- NROW(x) - NAs

  # Initialize result vector 
  result <- double(NROW(x))

  if(cumulative) {
    result[beg:NROW(x)] <- cumsum(x[beg:NROW(x)])
  } else {
    result[(n+beg-1)] <- sum(x[beg:(n+beg-1)])

    # Call Fortran routine
    result <- .Fortran( "runsum",
                     ia = as.double(x[beg:NROW(x)]),
                     lia = as.integer(len),
                     n = as.integer(n),
                     oa = as.double(result[beg:NROW(x)]),
                     loa = as.integer(len),
                     PACKAGE = "TTR",
                     DUP = TRUE )$oa
    
    # Prepend NAs from original data
    result <- c( rep( NA, NAs ), result )
  }
  
  # Replace 1:(n-1) with NAs
  is.na(result) <- c(1:(n-1+NAs))

  # Convert back to original class
  reclass(result, x)
}

#-------------------------------------------------------------------------#

#'@rdname runFun
#'@export
"runMin" <-
function(x, n=10, cumulative=FALSE) {

  x <- try.xts(x, error=as.matrix)

  if( n < 1 || n > NROW(x) ) stop("Invalid 'n'")

  if(NCOL(x) > 1) {
    stop("ncol(x) > 1. runMin only supports univariate 'x'")
  }

  # Count NAs, ensure they're only at beginning of data, then remove.
  NAs <- sum( is.na(x) )
  if( NAs > 0 ) {
    if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
    if( NAs + n > NROW(x) ) stop("not enough non-NA values")
  }
  beg <- 1 + NAs
  len <- NROW(x) - NAs

  # Initialize result vector 
  result <- double(NROW(x))
  
  if(cumulative) {
    result[beg:NROW(x)] <- cummin(x[beg:NROW(x)])
  } else {
    result[(n+beg-1)] <- min(x[beg:(n+beg-1)])

    result <- .Fortran( "runmin",
                     ia = as.double(x[beg:NROW(x)]),
                     lia = as.integer(len),
                     n = as.integer(n),
                     oa = as.double(result[beg:NROW(x)]),
                     loa = as.integer(len),
                     PACKAGE = "TTR",
                     DUP = TRUE )$oa

    # Prepend NAs from original data
    result <- c( rep( NA, NAs ), result )
  }
  
  # Replace 1:(n-1) with NAs
  is.na(result) <- c(1:(n-1+NAs))

  # Convert back to original class
  reclass(result, x)
}

#-------------------------------------------------------------------------#

#'@rdname runFun
#'@export
"runMax" <-
function(x, n=10, cumulative=FALSE) {

  x <- try.xts(x, error=as.matrix)
  
  if( n < 1 || n > NROW(x) ) stop("Invalid 'n'")

  # Count NAs, ensure they're only at beginning of data, then remove.
  NAs <- sum( is.na(x) )
  if( NAs > 0 ) {
    if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
    if( NAs + n > NROW(x) ) stop("not enough non-NA values")
  }
  beg <- 1 + NAs
  len <- NROW(x) - NAs

  if(NCOL(x) > 1) {
    stop("ncol(x) > 1. runMax only supports univariate 'x'")
  }

  # Initialize result vector 
  result <- double(NROW(x))

  if(cumulative) {
    result[beg:NROW(x)] <- cummax(x[beg:NROW(x)])
  } else {
    result[(n+beg-1)] <- max(x[beg:(n+beg-1)])

    result <- .Fortran( "runmax",
                     ia = as.double(x[beg:NROW(x)]),
                     lia = as.integer(len),
                     n = as.integer(n),
                     oa = as.double(result[beg:NROW(x)]),
                     loa = as.integer(len),
                     PACKAGE = "TTR",
                     DUP = TRUE )$oa
  }

  # Replace 1:(n-1) with NAs and prepend NAs from original data
  is.na(result) <- c(1:(n-1))
  result <- c( rep( NA, NAs ), result )

  # Convert back to original class
  reclass(result, x)
}

#-------------------------------------------------------------------------#

#'@rdname runFun
#'@export
"runMean" <-
function(x, n=10, cumulative=FALSE) {

  if(cumulative) {
    result <- runSum(x, n, cumulative) / 1:NROW(x)
  } else {
    result <- runSum(x, n) / n
  }

  return(result)
}

#-------------------------------------------------------------------------#

#'@rdname runFun
#'@export
"runMedian" <-
function(x, n=10, non.unique="mean", cumulative=FALSE) {

  x <- try.xts(x, error=as.matrix)

  if( n < 1 || n > NROW(x) ) stop("Invalid 'n'")

  # Count NAs, ensure they're only at beginning of data, then remove.
  NAs <- sum( is.na(x) )
  if( NAs > 0 ) {
    if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
    if( NAs + n > NROW(x) ) stop("not enough non-NA values")
  }
  beg <- 1 + NAs
  len <- NROW(x) - NAs

  if(NCOL(x) > 1) {
    stop("ncol(x) > 1. runMedian only supports univariate 'x'")
  }

  # Non-unique median
  non.unique <- match.arg(non.unique, c('mean','max','min'))
  non.unique <- switch( non.unique, mean=0, max=1, min=-1 )
  
  # Call Fortran routine
  result <- .Fortran( "runmedian",
                   ia = as.double(x[beg:NROW(x)]),
                   n = as.integer(n),
                   oa = double(len),
                   la = as.integer(len),
                   ver = as.integer(non.unique),
                   cu = as.integer(cumulative),
                   PACKAGE = "TTR",
                   DUP = TRUE )$oa

  # Replace 1:(n-1) with NAs and prepend NAs from original data
  is.na(result) <- c(1:(n-1))
  result <- c( rep( NA, NAs ), result )

  # Convert back to original class
  reclass(result, x)
}

#-------------------------------------------------------------------------#

#'@rdname runFun
#'@export
"runCov" <-
function(x, y, n=10, use="all.obs", sample=TRUE, cumulative=FALSE) {

  x <- try.xts(x, error=as.matrix)
  y <- try.xts(y, error=as.matrix)
  if(is.xts(x) && is.xts(y)) {
    xy <- cbind(x,y)
  } else {
    xy <- cbind( as.vector(x), as.vector(y) )
  }

  if( n < 1 || n > NROW(x) ) stop("Invalid 'n'")

  if(NCOL(x) > 1 || NCOL(y) > 1) {
    stop("ncol(x) > 1 or ncol(y) > 1.",
         " runCov only supports univariate 'x' and 'y'")
  }

  # "all.obs", "complete.obs", "pairwise.complete.obs"

  # Count NAs, ensure they're only at beginning of data, then remove.
  xNAs <- sum( is.na(x) )
  yNAs <- sum( is.na(y) )
  NAs <- max( xNAs, yNAs )
  if( NAs > 0 ) {
    if( any( is.na(xy[-(1:NAs),]) ) ) stop("Series contain non-leading NAs")
    if( NAs + n > NROW(x) ) stop("not enough non-NA values")
  }
  beg <- 1 + NAs
  len <- NROW(xy) - NAs
  
  xCenter <- runMean(x, n, cumulative)
  xCenter[1:(NAs+n-1)] <- 0
  yCenter <- runMean(y, n, cumulative)
  yCenter[1:(NAs+n-1)] <- 0

  # Call Fortran routine
  result <- .Fortran( "runCov",
                   rs1 = as.double(x[beg:NROW(xy)]),
                   avg1 = as.double(xCenter[beg:NROW(xy)]),
                   rs2 = as.double(y[beg:NROW(xy)]),
                   avg2 = as.double(yCenter[beg:NROW(xy)]),
                   la = as.integer(len),
                   n = as.integer(n),
                   samp = as.integer(sample),
                   oa = double(len),
                   cu = as.integer(cumulative),
                   PACKAGE = "TTR",
                   DUP = TRUE )$oa

  # Replace 1:(n-1) with NAs and prepend NAs from original data
  is.na(result) <- c(1:(n-1))
  result <- c( rep( NA, NAs ), result )

  # Convert back to original class
  # Should the attributes of *both* x and y be retained?
  reclass(result, x)
}

#-------------------------------------------------------------------------#

#'@rdname runFun
#'@export
"runCor" <-
function(x, y, n=10, use="all.obs", sample=TRUE, cumulative=FALSE) {

  result <- runCov(x, y, n, use=use, sample=sample, cumulative) /
            ( runSD(x, n, sample=sample, cumulative) *
              runSD(y, n, sample=sample, cumulative) )

  return( result )
}

#-------------------------------------------------------------------------#

#'@rdname runFun
#'@export
"runVar" <-
function(x, y=NULL, n=10, sample=TRUE, cumulative=FALSE) {

  if(is.null(y)) y <- x
  result <- runCov(x, y, n, use="all.obs", sample=sample, cumulative)

  return( result )
}

#-------------------------------------------------------------------------#

#'@rdname runFun
#'@export
"runSD" <-
function(x, n=10, sample=TRUE, cumulative=FALSE) {

  result <- sqrt( runCov(x, x, n, use="all.obs",
                  sample=sample, cumulative) )

  return( result )
}

#-------------------------------------------------------------------------#

#'@rdname runFun
#'@export
"runMAD" <-
function(x, n=10, center=NULL, stat="median",
         constant=1.4826, non.unique="mean", cumulative=FALSE) {

  x <- try.xts(x, error=as.matrix)

  if( n < 1 || n > NROW(x) ) stop("Invalid 'n'")

  if(NCOL(x) > 1) {
    stop("ncol(x) > 1. runMAD only supports univariate 'x'")
  }

  # Count NAs, ensure they're only at beginning of data, then remove.
  NAs <- sum( is.na(x) )
  if( NAs > 0 ) {
    if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
    if( NAs + n > NROW(x) ) stop("not enough non-NA values")
  }
  beg <- 1 + NAs
  len <- NROW(x) - NAs
  
  if(is.null(center)) {
    center <- runMedian(x, n, cumulative=cumulative)
  }
  center[1:(NAs+n-1)] <- 0

  # Mean or Median absolute deviation?
  median <- match.arg(stat, c("mean","median"))
  median <- switch( stat, median=TRUE, mean=FALSE )

  # Non-unique median
  non.unique <- match.arg(non.unique, c('mean','max','min'))
  non.unique <- switch( non.unique, mean=0, max=1, min=-1 )
  
  # Call Fortran routine
  result <- .Fortran( "runMAD",
                   rs = as.double(x[beg:NROW(x)]),      # raw series
                   cs = as.double(center[beg:NROW(x)]), # center series
                   la = as.integer(len),                # length of input arrays
                   n = as.integer(n),                   # size of rolling window
                   oa = double(len),                    # output array
                   stat = as.integer(median),           # center statistic
                   ver = as.integer(non.unique),        # median type
                   cu = as.integer(cumulative),         # from inception
                   PACKAGE = "TTR",
                   DUP = TRUE )$oa

  if( median ) result <- result * constant

  # Replace 1:(n-1) with NAs and prepend NAs from original data
  is.na(result) <- c(1:(n-1))
  result <- c( rep( NA, NAs ), result )

  # Convert back to original class
  reclass(result, x)
}

#-------------------------------------------------------------------------#

#'@rdname runFun
#'@export
"wilderSum" <-
function(x, n=10) {

  x <- try.xts(x, error=as.matrix)

  if( n < 1 || n > NROW(x) ) stop("Invalid 'n'")

  if(NCOL(x) > 1) {
    stop("ncol(x) > 1. wilderSum only supports univariate 'x'")
  }

  # Check for non-leading NAs
  # Leading NAs are handled in the C code
  x.na <- naCheck(x, n)

  # Call C routine
  result <- .Call("wilderSum", x, n, PACKAGE = "TTR")

  # Convert back to original class
  reclass(result, x)
}
