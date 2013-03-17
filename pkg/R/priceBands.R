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

#'Construct (optionally further smoothed and centered ) volatility bands around
#'prices
#'
#'John Bollinger's famous adaptive volatility bands most often use the typical
#'price of an HLC series, or may be calculated on a univariate price series
#'(see \code{\link{BBands}}).
#'
#'This function applies a second moving average denoted by \code{fastn} to
#'filter out higher-frequency noise, making the bands somewhat more stable to
#'temporary fluctuations and spikes.
#'
#'If \code{centered} is \code{TRUE}, the function also further smoothes and
#'centers the bands around a centerline adjusted to remove this higher
#'frequency noise.  If \code{lavg} is also \code{TRUE}, the smoothing applied
#'for the middle band (but not the volatility bands) is doubled to further
#'smooth the price-response function.
#'
#'If you have multiple different price series in \code{prices}, and want to use
#'this function, call this functions using \code{lapply(prices,PBands,...)}.
#'
#'@aliases PBands priceBands
#'@param prices A univariate series of prices.
#'@param n Number of periods to average over.
#'@param maType A function or a string naming the function to be called.
#'@param sd The number of standard deviations to use.
#'@param \dots any other pass-thru parameters, usually for function named by
#' \code{maType}.
#'@param fastn Number of periods to use for smoothing higher-frequency 'noise'.
#'@param centered Whether to center the bands around a series adjusted for high
#' frequency noise, default \code{FALSE}.
#'@param lavg Whether to use a longer \code{(n*2)} smoothing period for
#' centering, default \code{FALSE}.
#'@return A object of the same class as \code{prices} or a matrix (if
#'\code{try.xts} fails) containing the columns:
#' \describe{
#'    \item{ dn }{ The lower price volatility Band. }
#'    \item{ center }{ The smoothed centerline (see details). }
#'    \item{ up }{ The upper price volatility Band. }
#' }
#'@author Brian G. Peterson
#'@seealso \code{\link{BBands}}
#'@keywords ts
#'@examples
#'
#'   data(ttrc)
#'   pbands.close <- PBands( ttrc[,"Close"] )
#'
#'@rdname priceBands
#'@export
PBands <- function(prices, n=20, maType="SMA", sd=2, ..., fastn=2,
  centered=FALSE, lavg=FALSE ) {

  # Price Bands, implemented by Brian G. Peterson <brian@braverock.com>
  # inspired by the univariate Bollinger Bands, and Ram Ben-David

  if(!is.vector(prices) && ncol(prices)>1)
    stop('prices should be a univariate series, maybe use',
         'lapply(prices,PBands) instead?')
  
  prices <- try.xts(prices, error=as.matrix)

  # Default MA
  if(missing(maType)) {
    maType <- 'SMA'
  }

  maArgs <- list(n=n, ...)
  mavg  <- do.call( maType, c( list(prices), maArgs ) )
  
  maFastArgs <-list(n=fastn,...)
  fastmavg  <- do.call( maType, c( list(prices), maFastArgs ) )
  
  sdev <- runSD((mavg-fastmavg),n=n,sample=FALSE)
  
  if(!isTRUE(centered)){
    center <- mavg
  } else {
    centerrun <- (mavg-fastmavg)/sdev
    if(isTRUE(lavg)){
      maArgs <- list(n=(n*2), ...)
    }
    center <- mavg + ( do.call(maType, c( list(centerrun), maArgs ) ) )
  }
  
  up     <- center + sd * sdev
  dn     <- center - sd * sdev

  res <- cbind(dn, center, up)
  colnames(res) <- c("dn", "center", "up")

  reclass(res, prices)
}
