#
#   TTR: Technical Trading Rules
#
#   Copyright (C) 2007-2012  Joshua M. Ulrich
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
