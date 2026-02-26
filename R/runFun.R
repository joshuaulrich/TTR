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

#' Analysis of Running/Rolling/Moving Windows
#'
#' Various functions to analyze data over a moving window of periods.
#'
#'
#' @aliases runFun runSum runMin runMax runMean runMedian runCov runCor runVar
#' runSD runMAD wilderSum
#' @param x Object coercible to xts or matrix.
#' @param y Object coercible to xts or matrix.
#' @param n Number of periods to use in the window or, if
#' \code{cumulative=TRUE}, the number of observations to use before the first
#' result is returned. Must be between 1 and \code{nrow(x)}, inclusive.
#' @param cumulative Logical, use from-inception calculation?
#' @param sample Logical, sample covariance if \code{TRUE} (denominator of
#' \code{n-1})
#' @param use Only \code{"all.obs"} currently implemented.
#' @param non.unique One of 'mean', 'max', or 'min'; which compute their
#' respective statistics for the two middle values of even-sized samples.
#' @param center The values to use as the measure of central tendency, around
#' which to calculate deviations. The default (\code{NULL}) uses the median.
#' @param stat Statistic to calculate, one of 'median' or 'mean' (e.g. median
#' absolute deviation or mean absolute deviation, respectively.)
#' @param constant Scale factor applied to approximate the standard deviation.
#' @return A object of the same class as \code{x} and \code{y} or a vector (if
#' \code{try.xts} fails).
#'  \describe{
#'   \item{runSum}{returns sums over a n-period moving window.}
#'   \item{runMin}{returns minimums over a n-period moving window.}
#'   \item{runMax}{returns maximums over a n-period moving window.}
#'   \item{runMean}{returns means over a n-period moving window.}
#'   \item{runMedian}{returns medians over a n-period moving window.}
#'   \item{runCov}{returns covariances over a n-period moving window.}
#'   \item{runCor}{returns correlations over a n-period moving window.}
#'   \item{runVar}{returns variances over a n-period moving window.}
#'   \item{runSD}{returns standard deviations over a n-period moving window.}
#'   \item{runMAD}{returns median/mean absolute deviations over a n-period moving window.}
#'   \item{wilderSum}{retuns a Welles Wilder style weighted sum over a n-period moving window.}
#'  }
#'
#' @author Joshua Ulrich
#' @keywords ts
#' @rdname runFun
"runSum" <-
function(x, n=10, cumulative=FALSE) {

  x <- try.xts(x, error=as.matrix)

  if( n < 1 || n > NROW(x) )
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))

  if(NCOL(x) > 1) {
    stop("ncol(x) > 1. runSum only supports univariate 'x'")
  }

  if(cumulative) {
    # Count NAs, ensure they're only at beginning of data.
    NAs <- sum(is.na(x))
    if( NAs > 0 ) {
      if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
      if( NAs + n > NROW(x) ) stop("not enough non-NA values")
    }
    beg <- 1 + NAs

    # Initialize result vector
    result <- double(NROW(x))

    result[beg:NROW(x)] <- cumsum(x[beg:NROW(x)])

    # Replace 1:(n-1) with NAs
    is.na(result) <- seq_len(n-1+NAs)
  } else {
    # Call C routine
    result <- .Call(C_runsum, x, n)
  }

  # Convert back to original class
  reclass(result, x)
}

#-------------------------------------------------------------------------#

#' @rdname runFun
"runMin" <-
function(x, n=10, cumulative=FALSE) {

  x <- try.xts(x, error=as.matrix)

  if( n < 1 || n > NROW(x) )
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))

  if(NCOL(x) > 1) {
    stop("ncol(x) > 1. runMin only supports univariate 'x'")
  }

  if(cumulative) {
    # Count NAs, ensure they're only at beginning of data, then remove.
    NAs <- sum( is.na(x) )
    if( NAs > 0 ) {
      if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
      if( NAs + n > NROW(x) ) stop("not enough non-NA values")
    }
    beg <- 1 + NAs

    # Initialize result vector
    result <- double(NROW(x))

    result[beg:NROW(x)] <- cummin(x[beg:NROW(x)])

    # Replace 1:(n-1) with NAs
    is.na(result) <- seq_len(n-1+NAs)
  } else {
    # Call C routine
    result <- .Call(C_runrange, x, n)[, 1]
  }

  # Convert back to original class
  reclass(result, x)
}

#-------------------------------------------------------------------------#

#' @rdname runFun
"runMax" <-
function(x, n=10, cumulative=FALSE) {

  x <- try.xts(x, error=as.matrix)

  if( n < 1 || n > NROW(x) )
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))

  if(NCOL(x) > 1) {
    stop("ncol(x) > 1. runMax only supports univariate 'x'")
  }

  if(cumulative) {
    # Count NAs, ensure they're only at beginning of data, then remove.
    NAs <- sum( is.na(x) )
    if( NAs > 0 ) {
      if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
      if( NAs + n > NROW(x) ) stop("not enough non-NA values")
    }
    beg <- 1 + NAs

    # Initialize result vector
    result <- double(NROW(x))

    result[beg:NROW(x)] <- cummax(x[beg:NROW(x)])

    # Replace 1:(n-1) with NAs and prepend NAs from original data
    is.na(result) <- seq_len(n-1+NAs)
  } else {
    # Call C routine
    result <- .Call(C_runrange, x, n)[, 2]
  }

  # Convert back to original class
  reclass(result, x)
}

#' @rdname runFun
"runRange" <-
function(x, n=10, cumulative=FALSE) {

  x <- try.xts(x, error=as.matrix)

  if( n < 1 || n > NROW(x) )
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))

  if(NCOL(x) > 1) {
    stop("ncol(x) > 1. runMax only supports univariate 'x'")
  }

  if(cumulative) {
    # Count NAs, ensure they're only at beginning of data, then remove.
    NAs <- sum( is.na(x) )
    if( NAs > 0 ) {
      if( any( is.na(x[-(1:NAs)]) ) ) stop("Series contains non-leading NAs")
      if( NAs + n > NROW(x) ) stop("not enough non-NA values")
    }
    beg <- 1 + NAs

    if(NCOL(x) > 1) {
      stop("ncol(x) > 1. runRange only supports univariate 'x'")
    }

    # Initialize result matrix
    result <- matrix(NA_real_, nrow = NROW(x), ncol = 2)
    sub <- beg:NROW(x)
    result[sub,] <- cbind(cummin(x[sub]), cummax(x[sub]))
    # Replace 1:(n-1) with NAs and prepend NAs from original data
    is.na(result[seq_len(n-1+NAs), ]) <- TRUE
  } else {
    # Call C routine
    result <- .Call(C_runrange, x, n)
  }

  # Convert back to original class
  r <- reclass(result, x)
  colnames(r) <- c("min", "max")
  return(r)
}

#-------------------------------------------------------------------------#

#' @rdname runFun
"runMean" <-
function(x, n=10, cumulative=FALSE) {

  if(cumulative) {
    x.na <- sum(is.na(x))
    denom <- c(rep(NA_real_, x.na), seq_len(NROW(x)-x.na))
    result <- runSum(x, n, cumulative) / denom
  } else {
    result <- runSum(x, n) / n
  }

  return(result)
}

#-------------------------------------------------------------------------#

#' @rdname runFun
"runMedian" <-
function(x, n=10, non.unique="mean", cumulative=FALSE) {

  x <- try.xts(x, error=as.matrix)

  if( n < 1 || n > NROW(x) )
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))

  if(NCOL(x) > 1) {
    stop("ncol(x) > 1. runMedian only supports univariate 'x'")
  }

  # Non-unique median
  non.unique <- match.arg(non.unique, c('mean','max','min'))
  non.unique <- switch(non.unique, mean=0L, max=1L, min=-1L)

  # Call C routine
  result <- .Call(C_runmedian, x, n, non.unique, cumulative)

  # Convert back to original class
  reclass(result, x)
}

#-------------------------------------------------------------------------#

#' @rdname runFun
"runCov" <-
function(x, y, n=10, use="all.obs", sample=TRUE, cumulative=FALSE) {

  x <- try.xts(x, error=as.matrix)
  y <- try.xts(y, error=as.matrix)
  if(is.xts(x) && is.xts(y)) {
    xy <- cbind(x,y)
  } else {
    xy <- cbind( as.vector(x), as.vector(y) )
  }

  if( n < 1 || n > NROW(x) )
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))

  if(NCOL(x) > 1 || NCOL(y) > 1) {
    stop("ncol(x) > 1 or ncol(y) > 1.",
         " runCov only supports univariate 'x' and 'y'")
  }

  # "all.obs", "complete.obs", "pairwise.complete.obs"

  # Call C routine
  result <- .Call(C_runcov, xy[,1], xy[,2], n, sample, cumulative)

  # Convert back to original class
  # Should the attributes of *both* x and y be retained?
  reclass(result, x)
}

#-------------------------------------------------------------------------#

#' @rdname runFun
"runCor" <-
function(x, y, n=10, use="all.obs", sample=TRUE, cumulative=FALSE) {

  result <- runCov(x, y, n, use=use, sample=sample, cumulative) /
            ( runSD(x, n, sample=sample, cumulative) *
              runSD(y, n, sample=sample, cumulative) )

  return( result )
}

#-------------------------------------------------------------------------#

#' @rdname runFun
"runVar" <-
function(x, y=NULL, n=10, sample=TRUE, cumulative=FALSE) {

  if(is.null(y)) y <- x
  result <- runCov(x, y, n, use="all.obs", sample=sample, cumulative)

  return( result )
}

#-------------------------------------------------------------------------#

#' @rdname runFun
"runSD" <-
function(x, n=10, sample=TRUE, cumulative=FALSE) {

  result <- sqrt( runCov(x, x, n, use="all.obs",
                  sample=sample, cumulative) )

  return( result )
}

#-------------------------------------------------------------------------#

#' @rdname runFun
"runMAD" <-
function(x, n=10, center=NULL, stat="median",
         constant=1.4826, non.unique="mean", cumulative=FALSE) {

  x <- try.xts(x, error=as.matrix)

  if( n < 1 || n > NROW(x) )
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))

  if(NCOL(x) > 1) {
    stop("ncol(x) > 1. runMAD only supports univariate 'x'")
  }

  if(is.null(center)) {
    center <- runMedian(x, n, cumulative=cumulative)
  }

  # Mean or Median absolute deviation?
  median <- match.arg(stat, c("mean","median"))
  median <- switch( stat, median=TRUE, mean=FALSE )

  # Non-unique median
  non.unique <- match.arg(non.unique, c('mean','max','min'))
  non.unique <- switch( non.unique, mean=0, max=1, min=-1 )

  # Call C routine
  result <- .Call(C_runmad, x, center, n, median, non.unique, cumulative)

  if( median ) result <- result * constant

  # Convert back to original class
  reclass(result, x)
}

#-------------------------------------------------------------------------#

#' @rdname runFun
"wilderSum" <-
function(x, n=10) {

  x <- try.xts(x, error=as.matrix)

  if( n < 1 || n > NROW(x) )
    stop(sprintf("n = %d is outside valid range: [1, %d]", n, NROW(x)))

  if(NCOL(x) > 1) {
    stop("ncol(x) > 1. wilderSum only supports univariate 'x'")
  }

  # Check for non-leading NAs
  # Leading NAs are handled in the C code
  naCheck(x, n)  # called for error handling side-effect

  # Call C routine
  result <- .Call(C_wilderSum, x, n)

  # Convert back to original class
  reclass(result, x)
}
