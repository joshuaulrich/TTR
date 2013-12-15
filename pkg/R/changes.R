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

#'Rate of Change / Momentum
#'
#'Calculate the (rate of) change of a series over \code{n} periods.
#'
#'The ROC indicator provides the percentage difference of a series over two
#'observations, while the momentum indicator simply provides the difference.
#'
#'@aliases changes ROC momentum
#'@param x Price, volume, etc. series that is coercible to xts or matrix.
#'@param n Number of periods to use.
#'@param type Compounding type; either \code{"continuous"} (the default) or
#'\code{"discrete"}.
#'@param na.pad Should periods prior to \code{n} be appended?  Default is
#'\code{TRUE}.
#'@return A object of the same class as \code{x} or a vector (if \code{try.xts}
#'fails) containing the rate-of-change (or return) values for \code{ROC} or a
#'vector containing the differenced price series for \code{momentum}.
#'@author Joshua Ulrich
#'@keywords ts
#'@examples
#'
#' data(ttrc)
#' roc <- ROC(ttrc[,"Close"])
#' mom <- momentum(ttrc[,"Close"])
#'@rdname changes
#'@export
"ROC" <-
function(x, n=1, type=c("continuous","discrete"), na.pad=TRUE) {

  # Rate of Change

  x <- try.xts(x, error=as.matrix)
  type <- match.arg(type)

  if(is.xts(x)) {
    if(type=="discrete") {
      roc <- x / lag.xts(x,n,na.pad=na.pad) - 1
    }
    # Continuous change
    if(type=="continuous") {
      roc <- diff(log(x),n,na.pad=na.pad)
    }
    # Convert back to original class
    reclass(roc, x)
  } else {
    NAs <- NULL
    if(na.pad) {
      NAs <- rep(NA,n)
    }
    # Discrete changes
    if(type=="discrete") {
      roc <- c( NAs, x[(n+1):NROW(x)] / x[1:(NROW(x)-n)] - 1 )
    }
    # Continuous changes
    if(type=="continuous") {
      roc <- c( NAs, diff(log(x),n) )
    }
    return(roc)
  }
}

#-------------------------------------------------------------------------#

#'@rdname changes
#'@export
"momentum" <-
function(x, n=1, na.pad=TRUE) {

  # Momentum

  # http://www.fmlabs.com/reference/Momentum.htm
  # http://www.equis.com/Customer/Resources/TAAZ/?c=3&p=95
  # http://linnsoft.com/tour/techind/momentum.htm
  
  x <- try.xts(x, error=as.matrix)
  if(is.xts(x)) {
    mom <- diff(x,n,na.pad=na.pad)
  } else {
    NAs <- NULL
    if(na.pad) {
      NAs <- rep(NA,n)
    }
    mom <- c( NAs, diff(x, n) )
  }
  reclass(mom,x)
}
